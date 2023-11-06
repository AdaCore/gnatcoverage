"""
Check that the subprograms of interest mechanism works as expected, i.e.
produces the right coverage expectations for source traces, and the right error
message for binary traces.
"""

import os
import os.path

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

cov_args = build_and_run(
    gprsw=GPRswitches(
        gprfor(srcdirs=os.path.join("..", "src"), mains=["main.adb"])
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

# Produce a coverage report and a checkpoint with the subprogram switch. Check
# that the coverage report contains only coverage data for the specified
# subprograms for source traces. For binary traces, simply check that the
# gnatcov coverage invocation yields the expected warning.
os.mkdir("xcov_subp")
xcov(
    cov_args
    + [
        "--save-checkpoint",
        "trace.ckpt",
        "--subprograms",
        f"{os.path.join('..', 'src', 'pkg.ads')}:4",
        "--subprograms",
        f"{os.path.join('..', 'src', 'pkg.adb')}:10",
        "--subprograms",
        f"{os.path.join('..', 'src', 'pkg.adb')}:12",
        "--output-dir=xcov_subp",
    ],
    out="coverage.log",
)
if thistest.options.trace_mode == "bin":
    thistest.fail_if_no_match(
        "gnatcov coverage output",
        "warning: Ignoring --subprograms switches as this is not supported"
        " with binary traces.",
        contents_of("coverage.log"),
    )
else:
    check_xcov_reports(
        "*.xcov",
        {
            "main.adb.xcov": {},
            "pkg.adb.xcov": {"+": {6, 10, 12}},
        },
        "xcov_subp",
    )

    # Then check that the checkpoint contains only coverage data for the
    # specific subprogram. To do this, produce a new coverage report from the
    # checkpoint without using the --subprograms switch.
    xcov(
        cov_args[:-1]
        + ["--checkpoint", "trace.ckpt", "--output-dir=xcov_no_subp"]
    )
    check_xcov_reports(
        "*.xcov",
        {
            "main.adb.xcov": {"-": {5, 6}},
            "pkg.adb.xcov": {"+": {6, 10, 12}, "-": {11, 14, 15, 16}},
        },
        "xcov_no_subp",
    )

    # Also check the warnings when the subprogram switch is ill-formed

    # Case 1: missing colon in the argument
    xcov(
        cov_args + ["--subprograms", "no-colon"],
        out="xcov-wrong1.txt",
        register_failure=False,
    )
    thistest.fail_if_no_match(
        what="unexpected coverage output",
        regexp=r".*Wrong argument passed to --subprograms: .*",
        actual=contents_of("xcov-wrong1.txt"),
    )

    # Case 2: line number is not a number
    xcov(
        cov_args + [
            "--subprograms", f"{os.path.join('..', 'src', 'pkg.ads')}:b",
        ],
        out="xcov-wrong2.txt",
        register_failure=False,
    )
    thistest.fail_if_no_match(
        what="unexpected coverage output",
        regexp=r".*Wrong argument passed to --subprograms: .*",
        actual=contents_of("xcov-wrong2.txt"),
    )

    # Case 3: file does not exist
    xcov(
        cov_args + ["--subprograms", "dumb-file-name:4"],
        out="xcov-wrong3.txt",
        register_failure=False,
    )
    thistest.fail_if_no_match(
        what="unexpected coverage output",
        regexp=(
            r".*Error when parsing --subprograms argument dumb-file-name:4:"
            r".*dumb-file-name does not exist"
        ),
        actual=contents_of("xcov-wrong3.txt"),
    )


thistest.result()
