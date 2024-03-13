"""
Check that --warnings-as-errors works as expected.
"""

import re

from e3.fs import mkdir, rm

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")


def check(slug, project, warning):
    """
    Check that "gnatcov instrument" on the given project yields the expected
    warning, and that it exits with an error when --warnings-as-errors is
    passed.
    """

    # Run "gnatcov instrument" once with default arguments, just to check that
    # gnatcov succeeds and emits a warning.
    thistest.log(f"== {slug}: sanity check ==")
    log = f"{slug}-sc.txt"
    process = xcov_instrument(
        gprsw=GPRswitches(root_project=project),
        covlevel="stmt",
        register_failure=False,
        out=log,
    )
    thistest.fail_if(process.status != 0, "'gnatcov instrument' exit in error")
    thistest.fail_if_no_match(
        "'gnatcov instrument' output", warning, contents_of(log)
    )

    # Then pass --warnings-as-errors to check the exit code
    thistest.log(f"== {slug}: warnings-as-errors ==")
    log = f"{slug}-err.txt"
    process = xcov_instrument(
        gprsw=GPRswitches(root_project=project),
        covlevel="stmt",
        extra_args=["--warnings-as-errors"],
        register_failure=False,
        out=log,
    )
    thistest.fail_if(
        process.status == 0, "'gnatcov instrument' exit without error"
    )
    thistest.fail_if_no_match(
        "'gnatcov instrument' output", warning, contents_of(log)
    )


# Check the handling of warnings emitted before command line options (including
# --warnings-as-errors) are fully loaded. It also happens to be emitted by
# LibGPR.
#
# To achieve that, create a project with one listed source directory that does
# not exist and run "gnatcov instrument" on it: we expect a warning that
# complains about the missing directory. Note that we expect the warning twice:
# once for the wrapper gnatcov program, and one for the gnatcov64 program (both
# need to load the project).
#
# In order to create that project, we need to temporarily create that source
# directory so that gprfor does not strip it automagically.
missing_dir = "foobar"
mkdir(missing_dir)
project = gprfor(
    prjid="missing_srcdir",
    mains=["main.adb"],
    srcdirs=["../missing-srcdir", missing_dir],
)
rm(missing_dir, recursive=True)
check(
    "missing-srcdir",
    project,
    '.*warning: "foobar" is not a valid directory'
    '\n\n.*warning: "foobar" is not a valid directory\n',
)

# Create a project with a missing source file, on which the Ada instrumenter
# will complain. This checks the handling of warnings emitted after command
# line options are fully loaded.
check(
    "missing-srcfile",
    gprfor(
        prjid="missing_srcfile",
        mains=["main.adb"],
        srcdirs=["../missing-srcfile"],
    ),
    re.escape(
        "warning: While instrumenting main.adb..."
        "\nwarning: Cannot find required source file: pkg.ads"
    ),
)

thistest.result()
