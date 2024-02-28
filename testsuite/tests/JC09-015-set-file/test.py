from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


Wdir("tmp_")
xcov(["dump-lines", "../hello.o"], "dwarf.out")
thistest.result()
