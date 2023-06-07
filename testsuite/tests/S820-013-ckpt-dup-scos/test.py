"""
Test that checkpoints consolidation does not crash in the presence of "null"
SCOs, i.e. duplicated SCOs removed from the SCO vector in
Process_Low_Level_SCOs.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


wd = Wdir('tmp_')

exe = exepath_to('main')
trace = tracename_for('main')
gpr = gprfor(['main.c'], srcdirs='..')

# Build the test program and generate a trace file
gprbuild(gpr)
xrun([exe])

# Load the duplicated ALIs and create a checkpoint
ckpt = 'c.ckpt'
xcov(['coverage', '-c', 'stmt', '--save-checkpoint', ckpt,
      '--scos=obj/main.c.gli', '--scos=obj/foo.c.gli',
      trace])

# Now try to generate a report from this checkpoint
xcov(['coverage', '-a', 'xcov', '-c', 'stmt', '--checkpoint', ckpt])

thistest.result()
