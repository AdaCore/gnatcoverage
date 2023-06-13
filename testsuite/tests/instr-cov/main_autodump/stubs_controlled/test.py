"""
Check that the instrumentation of Ada mains is compatible with both the
presence of stub declarations in the declarative part of the main, and the
presence of controlled types in the main.
"""

from SCOV.instr import available_ada_dump_triggers
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir()

# Explicitly test all available dump triggers to maximize coverage
for dump_trigger in available_ada_dump_triggers():
    tmp.to_subdir(f"tmp_{dump_trigger}")
    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=[".."], mains=["main.adb"])
        ),
        covlevel="stmt",
        mains=["main"],
        dump_trigger=dump_trigger,
        extra_coverage_args=["-axcov"],
        trace_mode="src",
    )

    # We expect all lines to be covered
    check_xcov_reports(
        "*.xcov",
        {
            "main.adb.xcov": {"+": {10, 13, 15}},
            "main-inner.adb.xcov": {"+": {5}},
            "pkg.adb.xcov": {"+": {6}},
            "pkg.ads.xcov": {"+": {5}},
        },
        cwd="obj",
    )

thistest.result()
