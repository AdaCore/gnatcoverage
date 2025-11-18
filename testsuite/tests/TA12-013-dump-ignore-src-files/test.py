"""
Check that --dump-units-to shows the individually excluded source files.
This extends test T611-027-dump-units-to.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Generate a project, instrument it and run it
p = gprfor(mains=["main.adb"], srcdirs=[".."], langs=["Ada", "C"])
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
)
xcov_args_no_trace = xcov_args[:-1]
trace_file = xcov_args[-1]


def check_output(output_file, expected_content, regexp=False):
    """
    Check that the content of the "output_file" text file matches
    "expected_content". Check for equality if "regexp" is False and consider
    that "expected_content" is a regexp otherwise.

    :param str output_file: Name of the text file whose content needs to be
        checked.
    :param str expected_content: Expected content for this file.
    :param bool regexp: Whether to match as a regexp (by default, check
        content equality).
    """
    checker = (
        thistest.fail_if_no_match if regexp else thistest.fail_if_not_equal
    )
    checker(
        '"gnatcov coverage" output ({})'.format(output_file),
        expected_content,
        # Canonicalize to Unix-style line endings to have cross-platform checks
        contents_of(output_file).replace("\r\n", "\n"),
    )


def run_and_check(
    args, output_file, expected_content, regexp=False, success_expected=True
):
    """
    Run gnatcov with the given arguments.

    :param str output_file: Name of the temporary file to store the content of
        gnatcov's standard output.
    :param str expected_content: See the corresponding argument in "check".
    :param bool regexp: See the corresponding argument in "check".
    :param bool success_expected: If true, the test fails if gnatcov exits with
        an error code. If false, the test fails if gnatcov exits with the zero
        status code.
    """
    p = xcov(args, out=output_file, register_failure=success_expected)
    if not success_expected:
        thistest.fail_if(
            p.status == 0,
            'the call to "gnatcov coverage" was expected to fail, yet it'
            " succeeded (see {})".format(output_file),
        )
    check_output(output_file, expected_content, regexp)


# Check that nothing is dumped without --dump-units-to
run_and_check(xcov_args + ["-axcov"], "xcov-nodump.txt", "")

# Check that when *not* using the "--excluded-source-files" option, the list of
# units is still printed.
run_and_check(
    xcov_args
    + ["-axcov", "--dump-units-to=-", "--save-checkpoint=all_files.ckpt"],
    "xcov-stdout1.txt",
    expected_content=("foo.c\n" "identity.h\n" "main\n" "pkg\n" "pkh\n"),
)
run_and_check(
    xcov_args + ["-axcov", "--units=main", "--dump-units-to=-"],
    "xcov-stdout2.txt",
    expected_content="main\n",
)

# Check that dumping the list of units with excluded source files works on the
# "report" output.
REPORT_PATTERN = (
    "(.|\n)*=+"
    "\n== 4\\. UNITS OF INTEREST =="
    "\n=+"
    "\n"
    "\nfoo.c"
    "\nidentity.h"
    "\n   identity.h always ignored"
    "\nmain"
    "\npkg"
    "\n   pkg-test.adb always ignored"
    "\npkh"
    "\n   pkh-test.adb always ignored"
    "\n"
    "\n\\*\\* END OF REPORT \\*\\*"
    "\n(.|\n)*"
)
run_and_check(
    xcov_args
    + [
        "-areport",
        "--dump-units-to=-",
        "--excluded-source-files=*-test.adb",
        "--excluded-source-files=identity.h",
    ],
    "report-stdout.txt",
    REPORT_PATTERN,
    regexp=True,
)

# Check that the units of separates of level deeper than one is listed under
# the correct unit.
run_and_check(
    xcov_args
    + [
        "-axcov",
        "--dump-units-to=-",
        "--excluded-source-files=pkg-test-sep.adb",
    ],
    "xcov-stdout3.txt",
    expected_content=(
        "foo.c\n"
        "identity.h\n"
        "main\n"
        "pkg\n"
        "   pkg-test-sep.adb always ignored\n"
        "pkh\n"
    ),
)

# Check that a file ignored but that isn't part of a unit of interest
# is not listed.
run_and_check(
    xcov_args
    + [
        "-axcov",
        "--dump-units-to=-",
        "--units=pkg",
        "--excluded-source-files=*-test.adb",
        "--excluded-source-files=identity.h",
        "--save-checkpoint=pkg.ckpt",
    ],
    "xcov-stdout4.txt",
    expected_content=("pkg\n" "   pkg-test.adb always ignored\n"),
)

# Check that a file ignored that is part of a unit of interest is listed
run_and_check(
    xcov_args
    + [
        "-axcov",
        "--dump-units-to=-",
        "--units=pkh",
        "--excluded-source-files=pkh-test.adb",
        "--save-checkpoint=pkh.ckpt",
    ],
    "xcov-stdout5.txt",
    expected_content=("pkh\n" "   pkh-test.adb always ignored\n"),
)

# Check that loading results from a checkpoint shows files that were ignored
# when creating the checkpoint.
run_and_check(
    xcov_args_no_trace + ["-axcov", "--dump-units-to=-", "-Cpkh.ckpt"],
    "xcov-stdout6.txt",
    expected_content=(
        "foo.c\n"
        "identity.h\n"
        "main\n"
        "pkg\n"
        "pkh\n"
        "   pkh-test.adb always ignored\n"
    ),
)

run_and_check(
    xcov_args_no_trace
    + ["-axcov", "--dump-units-to=-", "-Cpkg.ckpt", "-Cpkh.ckpt"],
    "xcov-stdout7.txt",
    expected_content=(
        "foo.c\n"
        "identity.h\n"
        "main\n"
        "pkg\n"
        "   pkg-test.adb always ignored\n"
        "pkh\n"
        "   pkh-test.adb always ignored\n"
    ),
)

# Check that combining results from runs were a file was ignored in some
# of them but not all displays the files as 'sometimes ignored'.
run_and_check(
    xcov_args_no_trace
    + [
        "-axcov",
        "--dump-units-to=-",
        "-Cpkg.ckpt",
        "-Call_files.ckpt",
    ],
    "xcov-stdout8.txt",
    expected_content=(
        "foo.c\n"
        "identity.h\n"
        "main\n"
        "pkg\n"
        "   pkg-test.adb sometimes ignored\n"
        "pkh\n"
    ),
)

# Check that --excluded-source-files filters trace analysis but not
# checkpoints, and that if a file is ignored when analyzing a trace but is
# present in a checkpoint, then it is marked as 'sometimes ignored'.
run_and_check(
    xcov_args
    + [
        "-axcov",
        "--excluded-source-files=pkh-test.adb",
        "--save-checkpoint=pkh_not_pkg.ckpt",
    ],
    "xcov-sdtout9.txt",
    "",
)
run_and_check(
    xcov_args
    + [
        "-axcov",
        "--dump-units-to=-",
        "--excluded-source-files=*test.adb",
        "--excluded-source-files=identity.h",
        "-Cpkh_not_pkg.ckpt",
    ],
    "xcov-stdout9.txt",
    expected_content=(
        "foo.c\n"
        "identity.h\n"
        "   identity.h sometimes ignored\n"
        "main\n"
        "pkg\n"
        "   pkg-test.adb sometimes ignored\n"
        "pkh\n"
        "   pkh-test.adb always ignored\n"
    ),
)

thistest.result()
