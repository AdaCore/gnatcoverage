"""
Check that the 'dump-cfg' and 'disassemble-insn-properties' subcommands handle
well instructions at the end of the address space.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, gprbuild, xcov

tmp_ = Wdir('tmp_')

# We want to link the code at the end of the address space, so get the correct
# upper address depending on the architecture.

address = (
    2**32 - 1
    if '32bits' in thistest.options.tags else
    2**64 - 1
)

gprfile = gprfor(
    ['foo.s'],
    srcdirs="..",
    langs=('Asm'),
    extra='''
    package Linker is
        for Default_Switches ("Asm") use ("-nostdlib", "-Ttext={}");
    end Linker;
'''.format(address))
exefile = 'foo'

gprbuild(gprfile)

for subcmd in ('dump-cfg', 'disassemble-insn-properties'):
    # As long as GNATcov do not crash/hangs, everything is fine!
    xcov([subcmd, exefile, '_start'])

thistest.result()
