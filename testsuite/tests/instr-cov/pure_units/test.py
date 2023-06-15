"""
Check that we can compute code coverage for Pure units.
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
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode='src')
check_xcov_reports(
    '*.xcov',
    {'pkg.ads.xcov': {'+': {12, 15}, '-': {11, 14}},
     'pkg.adb.xcov': {'+': {5, 6}, '-': {8}},
     'pkg-child.ads.xcov': {'+': {9}, '-': {10, 12, 13}}},
    "xcov"
)

thistest.result()
