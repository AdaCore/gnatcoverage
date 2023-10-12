"""Check that the -P and -X command line args are passed to gnatemu."""

import re

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import exepath_to, gprbuild, gprfor, xrun


# Work in a subdirectory
wd = Wdir(subdir='tmp_')

# Build a program expected to run fine for our current target
gpr = gprfor(mains=['foo.adb'], srcdirs=['..'])
gprbuild(project=gpr)

program_path = exepath_to('foo')

# Pass a correct -P and fake -X options together with -v. Check that the -P
# and -X options are found in the gnatemu command executed, as displayed by
# -v. Beware that the -P option may be passed down in various ways, e.g.
# -P gen.gpr or -Pgen.gpr, regardless of how we pass it to gnatcov run.
xargs = ['-XFOO=foo1', '-Xbar=bar1']
xcovargs = ['-P', gpr, '-v', program_path] + xargs

p = xrun(xcovargs)
for arg in xargs + ["'-P' '*%s'" % re.escape(gpr)]:
    thistest.fail_if(
        not re.search('gnatemu.*%s' % arg, p.out),
        'couldn\'t find "%s" on the gnatemu command line out of %s' %
        (arg, ' '.join(xcovargs)))
thistest.result()
