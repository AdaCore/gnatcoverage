"""
Test that when using manual dump trigger in Ada with a specific pragma
indicating where to dump the coverage buffers, gnatcov is able to replace it
with a call to the dump buffers procedure and output correct traces.
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
                )

lib2_p = gprfor(mains=[],
                prjid="lib2",
                srcdirs="../src-lib2",
                objdir="obj-lib2",
                langs=["Ada"],
                )

p = gprfor(mains=["main.adb"],
           srcdirs=["../src"],
           objdir="obj",
           deps=["lib1", "lib2"])

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p, units=["lib1", "main"]),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
    dump_trigger="manual",
    manual_prj_name="gen")

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
    'xcov/main.adb.xcov': {'+': {12, 15, 19, 24, 26, 27, 28},
                           '-': {21, 29}},
    'xcov/lib1.adb.xcov': {'+': {4, 7}}})

thistest.result()
