"""
Check that we output clang diagnostics on source of interests only.
"""

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import thistest, gprfor
from SUITE.gprutils import GPRswitches

Wdir("tmp_")

mkdir("obj")

output = "instr.log"
xcov_instrument(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."],
                                          mains=["test.cpp"])),
    covlevel="stmt",
    out=output,
)

thistest.fail_if_no_match(
    "'gnatcov instrument' output",
    ".* error: default argument references parameter 'b'",
    contents_of(output).strip(),
)


thistest.result()
