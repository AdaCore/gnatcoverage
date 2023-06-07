from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import exepath_to, tracename_for, xcov, xrun


Wdir('tmp_')

# gprbuild('test_cond.gpr')
xrun(exepath_to('../test_cond'))
xcov(['coverage', '--level=branch', '--annotate=asm',
      tracename_for('test_cond')], 'cond.out')
thistest.fail_if(not match('40001ffc >:  32 80 00 02      bne,a', 'cond.out'),
                 'branch should be taken')

thistest.result()
