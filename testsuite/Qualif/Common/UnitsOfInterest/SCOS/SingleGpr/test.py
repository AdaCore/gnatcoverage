from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor

from test_support import check

wd = Wdir('wd_')
gprbuild(
    gprfor(srcdirs=['../src',            # For the test sources
                    '../../../../src'],  # For the support sources
           mains=['test_ab.adb']))

check(
    test_ali='obj/test_ab.ali',
    mon_ali='obj/monitor.ali')

thistest.result()
