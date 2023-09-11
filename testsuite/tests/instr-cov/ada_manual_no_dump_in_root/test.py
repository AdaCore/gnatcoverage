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

lib1_p = gprfor(mains=[],
                prjid="lib1",
                srcdirs="../src-lib1",
                objdir="obj-lib1",
                langs=["Ada"],
                deps=["lib3"]
                )

lib2_p = gprfor(mains=[],
                prjid="lib2",
                srcdirs="../src-lib2",
                objdir="obj-lib2",
                langs=["Ada"],
                )

lib2_p = gprfor(mains=[],
                prjid="lib3",
                srcdirs="../src-lib1/src-lib3",
                objdir="obj-lib3",
                langs=["Ada"],
                )

p = gprfor(mains=["main.adb"],
           srcdirs=["../src"],
           objdir="obj",
           deps=["lib1", "lib2", "lib3"])

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p, units=["lib1", "lib3"]),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
    dump_trigger="manual",
    manual_prj_name="lib1")

# Check that gnatcov inserted the call to the dump buffers procedure in the
# lib2.adb which is not a unit of interest


def check_call(file):
    thistest.fail_if_no_match("missing dump buffers procedure call",
                              "(\n|.)*GCVRT.DB_manual_lib2.Dump_Buffers;"
                              "(\n|.)*",
                              contents_of(file))


check_call('obj-lib2/lib2-gnatcov-instr/lib2.adb')

# Check that we get the expected coverage reports

check_xcov_reports('xcov/*.xcov', {
    'xcov/lib1.adb.xcov': {'+': {6, 8}, '-': {10}},
    'xcov/lib3.adb.xcov': {'+': {4, 6}}})

thistest.result()
