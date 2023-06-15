from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')
gprbuild(project=gprfor(['test_p.adb'], srcdirs='../src'))
xrun(exepath_to('test_p'))
xcov(['coverage', '--level=branch', '--annotate=xcov+',
      tracename_for('test_p')])

# We expect the test to exercise the single branch in p.head_of
# both ways, and that this branch is associated with the "if" statement
# there, something like
#
#    6 +:       if Blength > 0 then
thistest.fail_if(not match(r'[0-9]+ \+:.*if .* then', 'p.adb.xcov'))
thistest.result()
