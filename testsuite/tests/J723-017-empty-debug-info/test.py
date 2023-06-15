from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


Wdir('tmp_')
xcov(['dump-lines', '../ada_containers-aunit_lists.elf'], 'dwarf.out')
thistest.result()
