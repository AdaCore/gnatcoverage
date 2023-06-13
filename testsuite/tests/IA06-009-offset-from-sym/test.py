"""
Check that start of block addresses are offsets from the corresponding symbol,
not absolute addresses.
"""

from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.cutils import Wdir, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)

wd = Wdir('tmp_')
symbol = target_info().to_platform_specific_symbol('_ada_p')

gprbuild(gprfor(srcdirs=['../src'], mains=['p.adb']))
xrun(exepath_to('p'))
xcov(['coverage',
      '--level=insn', '--annotate=xcov+',
      '--routines={}'.format(symbol), tracename_for('p')])

thistest.fail_if(
    not match(r'<{}\+(00000000){{1,2}}>:'.format(symbol), 'p.adb.xcov'),
    'offset from symbols')
thistest.result()
