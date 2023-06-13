import os.path
import re

from SUITE.cutils import Wdir, contents_of, indent
from SUITE.tutils import (exepath_to, gprbuild, gprfor, thistest,
                          tracename_for, xcov, xrun)


root_dir = os.path.abspath('.')
wd = Wdir('tmp_')


gpr1 = gprfor(['foo.adb'], prjid='src1',
              srcdirs=os.path.join(root_dir, 'src1'),
              objdir='obj1', exedir='obj1')
gpr2 = gprfor(['foo.adb'], prjid='gpr',
              srcdirs=os.path.join(root_dir, 'src2'),
              objdir='obj2', exedir='obj2')
aggr = 'aggr.gpr'

with open(aggr, 'w') as f:
    f.write("""aggregate project Aggr is
    for Project_Files use ("{}", "{}");
end Aggr;
""".format(gpr1, gpr2))

exe1 = os.path.join('obj1', 'foo')
exe2 = os.path.join('obj2', 'foo')
trace1 = tracename_for(exe1)
trace2 = tracename_for(exe2)

gprbuild(aggr, extracargs=["-fdump-scos"])
xrun([exepath_to(exe1), '-o', trace1])
xrun([exepath_to(exe2), '-o', trace2])
xcov(['coverage', '-P', aggr, '-cstmt', '-areport', trace1, trace2],
     out='coverage.txt', register_failure=False)
log = contents_of('coverage.txt')

pattern = """\
.*gnatcov.*: error: the following source file:
  foo\\.adb
appears as the main source file in:
  .*obj1.foo.ali
  .*obj2.foo.ali
Is the same ALI file provided twice\\?\
"""
thistest.fail_if(
    not re.match(pattern, log, re.DOTALL),
    'Could not match "gnatcov coverage" output:\n'
    '{}\n'
    'against the expected pattern:\n'
    '{}\n'.format(indent(log), indent(pattern))
)

thistest.result()
