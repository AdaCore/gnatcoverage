"""
Check that gnatcov emits a fatal error when a main file does not contain the
main program.
"""

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

# Create object directory to avoid GPR warning
mkdir("obj")

p = xcov_instrument(
    GPRswitches(root_project=gprfor(srcdirs=['..'], mains=['main.c'])),
    covlevel='stmt',
    register_failure=False,
)
thistest.fail_if_not_equal('gnatcov instrument status code', 1, p.status)
thistest.fail_if_no_match(
    'gnatcov instrument output',
    '.*gnatcov.*: Could not find main function in main.c',
    contents_of('instrument.log').strip(),
)

thistest.result()
