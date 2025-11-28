"""
Check that the subprograms of interest mechanism works as expected, i.e.
produces the right coverage expectations for source traces, and the right error
message for binary traces.
"""

import os
import os.path
import re

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


src_traces = thistest.options.trace_mode == "src"


def check_xcov(label, args, expected_output=""):
    """
    Run xcov with the given aruments and check its output.

    Also pass it --output-dir={label}, and create that directory beforehand.
    """
    log = f"{label}.log"
    os.mkdir(label)
    xcov(args + [f"--output-dir={label}"], out=log)
    thistest.fail_if_not_equal(
        "'gnatcov coverage' output",
        expected_output,
        contents_of(log).strip(),
    )


tmp = Wdir("tmp_")

pkg_spec = os.path.join("..", "src", "pkg.ads")
pkg_body = os.path.join("..", "src", "pkg.adb")

thistest.log("== Checkpoint creation ==")
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
ckpt_filename = "trace.ckpt"
check_xcov(
    "xcov_subp",
    cov_args
    + [
        "--save-checkpoint",
        ckpt_filename,
        f"--subprograms={pkg_spec}:4",
        f"--subprograms={pkg_body}:19",
        f"--subprograms={pkg_body}:20",
    ],
    expected_output=(
        ""
        if src_traces
        else "warning: Ignoring --subprograms switches as this is not"
        " supported with binary traces."
    ),
)
cov_ckpt_args = cov_args[:-1] + ["--checkpoint", ckpt_filename]
if src_traces:
    check_xcov_reports(
        "xcov_subp",
        {
            "main.adb.xcov": {},
            "pkg.ads.xcov": {},
            "pkg.adb.xcov": {"+": {11, 19, 30}},
        },
    )

    # Then check that the checkpoint contains only coverage data for the
    # specific subprogram. To do this, produce a new coverage report from the
    # checkpoint without using the --subprograms switch.
    thistest.log("== xcov_no_subp ==")
    check_xcov("xcov_no_subp", cov_ckpt_args)
    check_xcov_reports(
        "xcov_no_subp",
        {
            "main.adb.xcov": {"-": {5, 6}},
            "pkg.ads.xcov": {"-": {7}},
            "pkg.adb.xcov": {"+": {11, 19, 30}, "-": {22, 33, 34, 35}},
        },
    )

    # Check that we can still select subprograms of interest declared in the
    # package body, when the package specification is ignored through
    # --excluded-source-files.
    thistest.log("== xcov_ignore ==")
    check_xcov(
        "xcov_ignore",
        cov_args
        + [
            f"--subprograms={pkg_body}:20",
            "--excluded-source-files=pkg.ads",
        ],
    )
    check_xcov_reports(
        "xcov_ignore",
        {
            "main.adb.xcov": {},
            "pkg.adb.xcov": {"+": {30}},
        },
    )

    # Also check the warnings when the subprogram switch is ill-formed

    # Case 1: missing colon in the argument
    thistest.log("== Missing colon ==")
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
    thistest.log("== Bad line number ==")
    xcov(
        cov_args + ["--subprograms", f"{pkg_spec}:b"],
        out="xcov-wrong2.txt",
        register_failure=False,
    )
    thistest.fail_if_no_match(
        what="unexpected coverage output",
        regexp=r".*Wrong argument passed to --subprograms: .*",
        actual=contents_of("xcov-wrong2.txt"),
    )

    # Case 3: file does not exist
    thistest.log("== No such file ==")
    xcov(
        cov_args + ["--subprograms", "dumb-file-name:4"],
        out="xcov-wrong3.txt",
        register_failure=False,
    )
    thistest.fail_if_no_match(
        what="unexpected coverage output",
        regexp=(
            r".*Error when parsing --subprograms argument dumb-file-name:4:"
            r" unknown source file"
        ),
        actual=contents_of("xcov-wrong3.txt"),
    )

    # Case 4: scope does not exist
    thistest.log("== No such scope ==")
    xcov(
        cov_args + [f"--subprograms={pkg_body}:14"],
        out="xcov-wrong3.txt",
        register_failure=False,
    )
    thistest.fail_if_no_match(
        what="unexpected coverage output",
        regexp=(
            ".*Error when parsing --subprograms argument"
            f" {re.escape(pkg_body)}:14: unknown subprogram"
        ),
        actual=contents_of("xcov-wrong3.txt"),
    )


thistest.result()
