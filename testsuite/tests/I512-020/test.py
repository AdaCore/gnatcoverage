from SUITE.context import thistest
from SUITE.tutils import xcov


xcov(['disp-routines', 'ppcstart.elf'])
thistest.result()
