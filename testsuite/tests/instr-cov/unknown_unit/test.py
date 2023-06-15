"""
Check that "gnatcov coverage" properly report a source trace entry referencing
an unknown instrumented unit.
"""

import os
import os.path

from e3.fs import cp, mkdir

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import xcov


p_gpr = os.path.abspath('p.gpr')
obj_dir = os.path.abspath('obj')
unrelated_sid = 'unrelated_instr.sid'

tmp = Wdir('tmp_')

# Instrument the main, only to get an SID file that is unrelated to the units
# we want to cover (pkg).
xcov_instrument(
    gprsw=GPRswitches(root_project=p_gpr, units=['main']),
    gpr_obj_dir='obj',
    covlevel='stmt')
cp(os.path.join('..', 'obj', 'main.sid'), unrelated_sid)

# Instrument/build/run the project only for the "pkg" unit
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p_gpr, units=['pkg']),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=[],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode='src')
trace_file = xcov_args[-1]

# Try to produce a coverage report for it, but providing the wrong SID
mkdir('xcov')
p = xcov(['coverage', '-v', '-axcov', '--output-dir=xcov',
          '--level', 'stmt',
          '--sid', unrelated_sid, trace_file],
         out='coverage.log')
thistest.fail_if(
    'discarding source trace entry for unknown instrumented unit: body of pkg'
    not in lines_of('coverage.log'))

thistest.result()
