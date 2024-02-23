"""
Check that using --dump-trigger=manual properly ignores files for languages
unknown to gnatcov. This used to crash.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

prj_name = "prj"

prj=gprfor(
    mains=["main_ada.adb"],
    prjid=prj_name,
    langs=["Ada", "Asm"],
    srcdirs=[".."]
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    mains=["main_ada"],
    covlevel="stmt",
    extra_coverage_args=["-axcov"],
    dump_trigger="manual",
    manual_prj_name=prj_name
)

check_xcov_reports("obj", {"main_ada.adb.xcov": {"+": {9}}})

thistest.result()
