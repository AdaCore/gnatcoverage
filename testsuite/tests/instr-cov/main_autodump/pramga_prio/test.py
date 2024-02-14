"""
Check that the instrumentation of Ada mains is compatible with the presence of
a pragma Priority in the main. It should be left in the top level declarations
of the main, otherwise the main will fail to compile.
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
    check_xcov_reports("obj", {"main.adb.xcov": {"+": {10, 13, 16}}})

thistest.result()
