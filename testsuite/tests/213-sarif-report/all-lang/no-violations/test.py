"""
Check the validity of the SARIF report generated for a project in which no
coverage violations were found. The project contains Ada, C, and C++ sources.
"""

from SCOV.minicheck import build_run_and_coverage
from SCOV.sarif import check_sarif_report
from SUITE.context import thistest
from SUITE.cutils import Wdir
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

check_sarif_report("../ref.sarif", "obj/coverage.sarif")

thistest.result()
