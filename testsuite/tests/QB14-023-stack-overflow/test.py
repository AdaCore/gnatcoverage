import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, text_to_file
from SUITE.tutils import exepath_to, gprfor, gprbuild, xcov


wd = Wdir('tmp_')
main = exepath_to('foo')

gpr = gprfor(['foo.adb'], srcdirs='..')
gprbuild(gpr)

# Patch the SCOs so that a statement covers an insanely huge amount of lines
ali_file = os.path.join('obj', 'foo.ali')
text_to_file(contents_of(ali_file).replace('CS 3:4-3:4', 'CS 3:4-1000000:4'),
             ali_file)

# Just check that the following command does not crash (it used to yield a
# stack overflow).
xcov(['map-routines', '-P{}'.format(gpr), main])

thistest.result()
