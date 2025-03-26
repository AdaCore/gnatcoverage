"""
Test that "gnatcov setup" produces a coverage library compatible with the
-nostdlib builder switch, and warns on inappropriate configurations.
"""

import os.path

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import xcov


tmp = Wdir()


def error_case(tc_id, args, expected_out, expect_failure):
    """
    Run gnatcov setup with the given args, and check that the output matches
    expected_out. Additionally, check the return status of the setup command,
    which should match expect_failure.

    The command is run in a temporary subdirectory, with --prefix=. added to
    the gnatcov setup args to keep each check separate. id is used as a
    temporary directory suffix.

    -q is also added to args to filter out the user directions printed at the
    end of the process.
    """
    thistest.log(f"====================== {tc_id} =====================")
    tmp.to_subdir(f"tmp_{tc_id}")
    log = "setup.log"
    p = xcov(
        ["setup"] + args + ["-q", "--prefix=."],
        out=log,
        register_failure=not expect_failure,
        auto_config_args=False,
        tolerate_messages=".",
    )

    thistest.fail_if(
        expect_failure and p.status == 0,
        "expected 'gnatcov setup' to return a non-zero status code",
    )

    thistest.fail_if_no_match(
        "Unexpected 'gnatcov setup' output",
        regexp=expected_out,
        actual=contents_of(log),
    )


# First, test that gnatcov setup correctly warns about suspicious combinations
# of options.

error_case(
    tc_id="profile_incompatibility",
    args=["--no-stdlib", "--rts-profile=full"],
    expected_out=r'.*: RTS profile "full" not compatible with --no-stdlib',
    expect_failure=True,
)

error_case(
    tc_id="suspicious_dump_trigger",
    args=["--no-stdlib", "--dump-trigger=atexit", "--RTS=light"],
    expected_out="warning: --dump-trigger=atexit may not be compatible with"
    " the selected runtime",
    expect_failure=False,
)

error_case(
    tc_id="suspicious_dump_channel",
    args=["--no-stdlib", "--dump-channel=bin-file", "--RTS=light"],
    expected_out="warning: --dump-channel=bin-file may not be compatible with"
    " the selected runtime",
    expect_failure=False,
)

# Then check that we can actually build and get a trace from a program
# with Ada code but not linked against the Ada runtime. Do it twice, once with
# and once without and Ada runtime. We expect no coverage difference.
#
# In the code bellow, if use_nostdlib is True then the -nostdlib linker switch
# will be used to build the project, and we do not expect to find the Ada
# runtime library in the linker command.

for tc_id, use_nostdlib in [
    ("no_stdlib", True),
    ("with_stdlib", False),
]:
    thistest.log(f"=============== Cov {tc_id} ================")
    tmp.to_subdir(f"tmp_{tc_id}")
    cp(os.path.join("..", "src"), ".", recursive=True)
    cp(os.path.join("..", "prj.gpr"), ".")

    xcov(
        [
            "setup",
            "-q",
            "--prefix=cov_rts",
            "--RTS=light",
            "--no-stdlib",
        ],
        auto_config_args=False,
        out="setup.log",
    )

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project="prj.gpr",
            xvars=[("USE_NOSTDLIB", "yes" if use_nostdlib else "no")],
        ),
        covlevel="stmt+mcdc",
        mains=["main"],
        dump_trigger=None,
        dump_channel="base64-stdout",
        extra_coverage_args=["-axcov"],
        runtime_project=os.path.join(
            "cov_rts", "share", "gpr", "gnatcov_rts.gpr"
        ),
        auto_config_args=False,
        extra_gprbuild_args=["-v"],
    )

    # Check whether we have a libgnat in the gprbuild linker command in the
    # log, and compare that against the expected value for this run.
    found_libgnat = "libgnat.a" in contents_of("gprbuild.out")
    if use_nostdlib:
        thistest.fail_if(
            found_libgnat,
            "Found libgnat.a in gprbuild -v output",
        )
    else:
        thistest.fail_if(
            not found_libgnat,
            "Missing libgnat.a in gprbuild -v output",
        )

    check_xcov_reports(
        "obj",
        {
            "main.c.xcov": {"-": {11}, "+": {18, 24, 25}},
            "pkg.ads.xcov": {},
            "pkg.adb.xcov": {"-": {6}, "+": {8}, "!": {5}},
        },
    )

thistest.result()
