"""
Check that instrumentation with automatic dump works correctly when the main is
an Ada spec.
"""

import os.path

from SCOV.instr import available_ada_dump_triggers
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir()

main_sources = ["inst_main.ads", "pkg-child_main.ads"]
mains = [os.path.splitext(f)[0] for f in main_sources]

# Explicitly test all available dump triggers to maximize coverage
for dump_trigger in available_ada_dump_triggers():
    thistest.log(f"== {dump_trigger} ==")
    tmp.to_subdir(f"tmp_{dump_trigger}")

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(mains=main_sources, srcdirs=[".."]),
        ),
        covlevel="stmt",
        mains=mains,
        extra_coverage_args=["--annotate=xcov"],
    )
    check_xcov_reports(
        "*.xcov",
        {
            "generic_main.adb.xcov": {"+": {5}},
            "inst_main.ads.xcov": {},
            "pkg-child_main.ads.xcov": {},
        },
        "obj",
    )

thistest.result()
