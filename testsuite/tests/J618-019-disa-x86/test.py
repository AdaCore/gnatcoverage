from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


Wdir('tmp_')
xcov(['disassemble', '../insn.o'], 'insn.dis')
thistest.result()
