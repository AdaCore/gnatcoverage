"""
Check the validity of the SARIF report generated for a project in which no
coverage violations were found. The project contains Ada, C, and C++ sources.
"""
from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(
            prjid="test",
            srcdirs=[".."],
            mains=["main.adb"],
            langs=["Ada", "C", "C++"],
        )
    ),
    covlevel=("stmt"),
    trace_mode="src",
    mains=["main"],
    extra_coverage_args=["--annotate=sarif"],
)

thistest.fail_if_no_match(
    "SARIF report",
    contents_of("../ref.sarif"),
    contents_of("obj/coverage.sarif"),
)

thistest.result()
