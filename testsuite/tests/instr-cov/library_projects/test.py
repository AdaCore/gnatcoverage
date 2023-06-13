"""
Test that instrumented source coverage works as expected on a setup of library
projects and one program project.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches


main_gpr = os.path.abspath('main.gpr')
main_obj_dir = os.path.abspath('obj-main')

tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=main_gpr,
                      projects=['main', 'math', 'vectors']),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    gpr_obj_dir=main_obj_dir,
    gpr_exe_dir=main_obj_dir,
    trace_mode='src')
check_xcov_reports('xcov/*.xcov', {
    'xcov/main.adb.xcov': {'+': {5, 9, 11, 12, 13}},

    'xcov/vectors.ads.xcov':  {'+': {6, 15, 17, 18, 19, 20}},
    'xcov/vectors.adb.xcov':  {'+': {5, 10, 15, 18, 19},
                               '-': {16, 25, 26, 28}},

    'xcov/math.ads.xcov':  {'+': {3, 11, 12, 13}},
    'xcov/math.adb.xcov':  {'+': {5, 10}, '-': {15}},
})

thistest.result()
