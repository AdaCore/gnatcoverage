import glob
import os.path

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov


src_dir = os.path.abspath('.')
wd = Wdir('tmp_')


build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(['main.adb'], srcdirs=src_dir,
                                          objdir='.')),
    gpr_obj_dir='.',
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['--save-checkpoint=main.ckpt'],
    scos=['bar', 'foo', 'main'])
xcov(['coverage', '-cstmt', '-axcov', '-Cmain.ckpt'])

expected_reports = ['foo.adb.xcov', 'main.adb.xcov']
if thistest.options.trace_mode == "src":
    expected_reports += ['bar.ads.xcov', 'foo.ads.xcov']
expected_reports.sort()
got_reports = sorted(glob.glob('*.xcov'))
thistest.fail_if(
    got_reports != expected_reports,
    ('Expected the following reports: {}\n'
     'But got instead:                {}'.format(
         expected_reports, got_reports))
)

thistest.result()
