"""
Test that when using manual dump trigger in Ada with the appropriate buffers
dump pragma only in a subproject itself dependent on another subproject
gnatcov is able to provide the correct coverage analysis.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir('tmp_')


def make_lib_gpr(name, srcdirs, deps):
    return gprfor(mains=[],
                  prjid=name,
                  srcdirs=srcdirs,
                  objdir="obj-" + name,
                  langs=["Ada"],
                  deps=deps)


lib1_p = make_lib_gpr("lib1", "../src-lib1", ["lib2"])
lib2_p = make_lib_gpr("lib2", "../src-lib1/src-lib2", None)

p = gprfor(mains=["main.adb"],
           srcdirs=["../src"],
           objdir="obj",
           deps=["lib1", "lib2"])

# Check that we get the expected coverage reports

# Running gnatcov natively allows to have one source trace file per
# project.
instr_warning = (r"warning: Manual buffer dump/reset indications were"
                 r" found in.*")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p, units=["lib1", "lib2"]),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
    dump_trigger="manual",
    manual_prj_name="lib1",
    tolerate_instrument_messages=instr_warning)

check_xcov_reports('xcov', {
    'lib1.adb.xcov': {'+': {6, 8}, '-': {10}},
    'lib1.ads.xcov': {},
    'lib2.adb.xcov': {'+': {4, 6}},
    'lib2.ads.xcov': {},
})

thistest.result()
