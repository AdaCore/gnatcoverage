"""
Check that gnatcov correctly reinstrument a source containing a manual dump
indication.
"""

from SCOV.minicheck import (
    build_run_and_coverage,
    check_xcov_reports,
    xcov_instrument,
)
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

prjid = "gen"
root_prj = gprfor(prjid=prjid, srcdirs=[".."], mains=["main.adb"])

# The first instrumentation command should instrument everything
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    dump_trigger="manual",
    extra_args=["--units", "foo"],
)

# Then, the second instrumentation command should not instrument anything
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")
build_run_and_coverage(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="manual",
    manual_prj_name=prjid,
    extra_args=["--units", "foo"],
    extra_coverage_args=["-axcov"],
)
thistest.fail_if_diff(
    baseline_file="../instrument.expected",
    actual_file="instrument.log",
)

check_xcov_reports("obj", {"foo.adb.xcov": {"+": {3}}})
thistest.result()
