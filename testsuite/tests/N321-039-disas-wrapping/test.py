"""
Check that the 'dump-cfg' and 'disassemble-insn-properties' subcommands handle
well instructions at the end of the address space.
"""

from e3.os.process import Run

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import xcov

tmp_ = Wdir("tmp_")

# We want to link the code (4 bytes) at the end of the address space, so get
# the correct upper address depending on the architecture.

address = (2**32 if "32bits" in thistest.options.tags else 2**64) - 4

exefile = "foo"
log_file = "gcc.log"
p = Run(
    [
        "arm-eabi-gcc",
        "-o",
        exefile,
        "../foo.s",
        "-nostdlib",
        f"-Ttext={hex(address)}",
    ],
    output=log_file,
)
thistest.fail_if(p.status != 0, "gcc failed:\n" + contents_of(log_file))

for subcmd in ("dump-cfg", "disassemble-insn-properties"):
    # As long as GNATcov do not crash/hangs, everything is fine!
    xcov([subcmd, exefile, "_start"])

thistest.result()
