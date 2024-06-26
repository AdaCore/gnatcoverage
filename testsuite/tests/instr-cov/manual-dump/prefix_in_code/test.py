"""
Test that the prefix for the trace can be configured from sources, using
either

    pragma Annotate (Xcov, Dump_Buffers, Prefix);

or

    /* GNATCOV_DUMP_BUFFERS (prefix) */

where prefix designates any expression evaluating to an Ada String or a C
null-terminated char *.
"""

import glob

from SCOV.minicheck import build_and_run, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

expected_cov = {
    "ada": {
        "main_ada.adb.xcov": {"+": {4, 6}},
        "main_c.c.xcov": {"-": {6, 7, 9}},
    },
    "c": {
        "main_c.c.xcov": {"+": {6, 7}, "-": {9}},
        "main_ada.adb.xcov": {"-": {4, 6}},
    },
}


def check_one_trace(cov_args, lang):
    """
    Find the trace generated by the lang executable, assuming the trace prefix
    is {lang}_trace. Then create a coverage report from that trace and check we
    got the expected results.
    """

    thistest.log(f"======== Checking {lang} trace =========")

    # We expect the trace to be in the format
    # {lang}_trace-{unique-part}.srctrace:
    trace = glob.glob(f"{lang}_trace-*.srctrace")

    output_dir = f"output_{lang}/"
    xcov(
        cov_args + [f"--output-dir={output_dir}"] + trace,
        out=f"coverage_{lang}.log",
    )
    check_xcov_reports(output_dir, expected_cov[lang])


prj_id = "p"

# Instrument build and run
cov_args = build_and_run(
    gprsw=GPRswitches(
        gprfor(
            prjid=prj_id,
            srcdirs=[".."],
            mains=["main_ada.adb", "main_c.c"],
        )
    ),
    covlevel="stmt",
    mains=["main_ada", "main_c"],
    dump_trigger="manual",
    manual_prj_name=prj_id,
    extra_coverage_args=["-axcov"],
)

# Check the trace name and report for each executable
for lang in ["c", "ada"]:
    check_one_trace(cov_args, lang)

thistest.result()
