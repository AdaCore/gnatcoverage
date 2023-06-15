"""Regression testcase for the use of checkpoints and MC/DC coverage."""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov


tmp = Wdir('tmp_')

gpr = gprfor(['main1.adb', 'main2.adb'], srcdirs='..')

# Create a checkpoint for the execution of "main1" and another for "main2"
for i in (1, 2):
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=gpr),
        covlevel='stmt+mcdc',
        mains=['main{}'.format(i)],
        extra_coverage_args=['--save-checkpoint=c{}.ckpt'.format(i)],
        scos=['obj/main{}'.format(i)])

# Now consolidate both. c1.ckpt comes first in order to check that, when
# c2.ckpt is loaded, all SCOs in it, and in particular the ones in evaluation
# vectors for MC/DC, are properly remapped.
xcov(['coverage', '--annotate=xcov', '--level=stmt+mcdc',
      '--checkpoint=c1.ckpt', '--checkpoint=c2.ckpt'])

check_xcov_reports('*.xcov', {
    'main1.adb.xcov': {'+': {5}},
    'main2.adb.xcov': {'+': {5, 7, 11, 12, 13, 15}, '!': {4}}})

thistest.result()
