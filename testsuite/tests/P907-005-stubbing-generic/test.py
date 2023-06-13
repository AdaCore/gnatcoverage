import os
import os.path
import shutil

from SCOV.minicheck import (build_run_and_coverage, checked_xcov,
                            check_xcov_reports)
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches


class TestDriver(object):
    def __init__(self, test_dir, exename):
        self.test_dir = test_dir
        self.exename = exename

        self.obj_dir = os.path.join('..', test_dir, 'obj')
        self.prj_file = os.path.join('..', test_dir, 'test_runner.gpr')
        self.ckpt_file = exename + '.ckpt'


test_drivers = [TestDriver('test_p', 'tp1'),
                TestDriver('test_t', 'tt1'),
                TestDriver('test_sensors', 'ts1')]


# Cleanup any previous test run artefacts
for td in test_drivers:
    if os.path.exists(td.obj_dir):
        shutil.rmtree(td.obj_dir)


wd = Wdir('tmp_')


# Build projects, generate trace and checkpoints for each test drivers
for td in test_drivers:
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=td.prj_file),
        covlevel='stmt+decision',
        mains=[td.exename],
        gpr_obj_dir=td.obj_dir,
        gpr_exe_dir=td.obj_dir,
        extra_coverage_args=['--save-checkpoint', td.ckpt_file],
        out='ckpt-{}.log'.format(td.exename),
        scos_for_run=False)


# Now, generate the final report
ckpt_args = ['--checkpoint={}'.format(td.ckpt_file)
             for td in test_drivers]
checked_xcov(['coverage', '-P', os.path.join('..', 'sensors', 'sensors.gpr'),
              '--level=stmt+decision', '--annotate=xcov',
              '--output-dir=.'] + ckpt_args,
             'cons.log')

# Finally, check we have the expected reports
check_xcov_reports(
    '*.xcov',
    {
        'pressure_control.adb.xcov': {
            '+': {10, 18, 19},
            '!': {11},
            '-': {12},
        },
        'temperature_control.adb.xcov': {
            '+': {10, 18, 19},
            '!': {11},
            '-': {12},
        }
    }
)

thistest.result()
