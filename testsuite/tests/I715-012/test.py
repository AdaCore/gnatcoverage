import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_tmp
from SUITE.tutils import (exepath_to, gprbuild, gprfor,
                          platform_specific_symbols, tracename_for, xcov, xrun)


wd = Wdir('tmp_')
gprbuild(project=gprfor(srcdirs=['../src'], mains=['p.adb']))
symfile = list_to_tmp(platform_specific_symbols(['pack__func']))

xrun(exepath_to('p'))
xcov(['coverage', '--level=branch',
      '--routines=@' + symfile,
      '--annotate=xcov+', tracename_for('p')])

# This test should check that xcov does not generate null report files
# for sources that are not considered in the coverage operation.
# Here, only pack.adb.xcov should be generated, as the routine list
# contains only pack__func, fully defined in pack.adb.
thistest.fail_if(not os.path.exists('pack.adb.xcov')
                 or os.path.exists('text_io.adb.xcov')
                 or os.path.exists('monitor.ads.xcov')
                 or os.path.exists('monitor.adb.xcov')
                 or os.path.exists('p.adb.xcov'))

thistest.result()
