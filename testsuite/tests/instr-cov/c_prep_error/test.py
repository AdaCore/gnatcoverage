"""
Check that C/C++ preprocessing failures are properly reported.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

log_file = "instr.txt"
p = xcov_instrument(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel="stmt",
    out=log_file,
    register_failure=False,
)

# Make sure that preprocessing fails with exit code 1, that the output mentions
# the missing file (the bit that is useful to users in order to understand what
# went wrong), plus the "abort" message.

thistest.fail_if_not_equal("'gnatcov instrument' exit code", 1, p.status)

log = contents_of(log_file)
for excerpt in ["foo.hh", "Preprocessing failed: aborting"]:
    thistest.fail_if(
        excerpt not in log,
        f"Missing excerpt in {log_file}: {repr(excerpt)}"
    )

thistest.result()
