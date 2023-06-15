from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.cutils import Wdir, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')
extralargs = ',--entry=foo' if target_info().partiallinks else ''

gprbuild(gprfor(['foo.adb'], srcdirs='../src'),
         extracargs='-ffunction-sections',
         largs='-Wl,--gc-sections' + extralargs)
xrun(exepath_to('foo'))
xcov(['coverage', '--level=stmt',  '--annotate=xcov', '--scos=obj/pck.ali',
      tracename_for('foo')], out='ignore.out')

thistest.fail_if(not match(r'\.:       null; --  ELIMINATED', 'pck.adb.xcov'),
                 'eliminated code not reported')
thistest.fail_if(not match(r'\+:       null; --  COVERED', 'pck.adb.xcov'),
                 'code not eliminated not reported')
thistest.result()
