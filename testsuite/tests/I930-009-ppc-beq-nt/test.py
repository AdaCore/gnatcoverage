"""
This test is about a xcov/qemu misinteraction on very specific branch
sequences. We built once from the sources in src and re-exercise the binary.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import exepath_to, tracename_for, xcov, xrun


Wdir('tmp_')

# gprbuild(project=gprfor(['test_robots.adb']))
# gprbuild('test_cond.gpr')

# The test intently gets into the 'Run' function twice in Cautious mode for a
# safe command.  We expect, then, partial coverage for both conditions in
#
#   procedure Run (R : in out Robot; C : Command) is
#      Mode : Opmode;
#   begin
#      Mode := Current_Mode (R);
#      if Mode = Cautious         <==
#        and then Unsafe (C)      <==
#
# We seek the reported status of the associated cond branch instructions
# directly:

xrun(exepath_to('../test_cond'))
xcov(['coverage', '--level=branch', '--annotate=asm',
      tracename_for('test_cond')], 'cond.out')
thistest.fail_if(not match('fff01004 -:  41 9e 00 0c      beq', 'cond.out'),
                 'branch should not be taken')

thistest.result()
