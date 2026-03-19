"""
Test that instrumentation of a project tree including an extended project
produces valid sources, and that coverage for all units is properly dumped.
"""

import glob
import os

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

base_gpr = gprfor(
    mains=["main.adb"],
    srcdirs=["../src_base"],
    objdir="obj_base",
    prjid="p_base",
)
ext_gpr = "p_ext.gpr"

# gprfor does not allow the creation of an extend project, so use our own.
cp(os.path.join("..", ext_gpr), ".")

# Run the coverage cycle
build_run_and_coverage(
    gprsw=GPRswitches(ext_gpr),
    covlevel="stmt",
    mains=["main"],
    gpr_obj_dir="obj_ext",
    dump_trigger="manual",
    manual_prj_name="P_Ext",
    extra_coverage_args=["-axcov"],
)


# Check that there only is a single trace produced, to ensure that the manual
# dump indication in the overridden common.adb file has not been compiled.
traces = glob.glob("*.srctrace")
thistest.fail_if_not_equal(
    "Unexpected number of traces",
    expected=1,
    actual=len(traces),
)

# Check the coverage contents
check_xcov_reports(
    "obj_ext",
    {
        "main.adb.xcov": {"+": {5}},
        "common.ads.xcov": {},
        "common.adb.xcov": {},
        "contractor.ads.xcov": {},
        "contractor.adb.xcov": {"+": {7}},
    },
)

thistest.result()
