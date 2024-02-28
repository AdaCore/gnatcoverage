"""
Check that the PPC disassembler (the Disassemble_Insn procedure) handles well
input that is larger than a single instruction.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, gprbuild, xcov

tmp_ = Wdir("tmp_")

gprfile = gprfor(["foo.adb"], srcdirs="..")
gprbuild(gprfile)

# As long as GNATcov do not crash/hangs, everything is fine!
xcov(["disassemble-insn-properties", "foo", "_ada_foo"])

thistest.result()
