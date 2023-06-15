"""
Check that we properly instrument expression functions that are primitives
of tagged types for MC/DC.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches


p_gpr = os.path.abspath('p.gpr')
obj_dir = os.path.abspath('obj')

tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p_gpr),
    covlevel='stmt+uc_mcdc',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode='src')

# TODO: update coverage expectations once compiler bug
# has been fixed and XFAIL is removed.

check_xcov_reports('xcov/*.xcov', {'xcov/main.adb.xcov': {
    '+': {4, 5, 7, 12, 14, 17, 19, 21, 22, 23, 24}
}})

thistest.result()
