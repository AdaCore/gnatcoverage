"""
Check that gnatcov coverage -o produces report in output file for =asm or
=report requests.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')

pgm = 'plop'
pgreport = pgm + '.cov'
pgout = pgm + '.stdout'

gprbuild(project=gprfor(pgm + '.adb', srcdirs='../src'))
xrun(exepath_to(pgm))


def check_output(spec):
    fmt = spec['fmt']
    level = spec['level']
    key = spec['key']

    xcov('coverage --level=%s --annotate=%s --scos=%s -o %s %s' % (
           level, fmt, 'obj/' + pgm + '.ali', pgreport, tracename_for(pgm)),
         out=pgout)
    thistest.fail_if(
        not match(key, pgreport),
        'expect asm dump in -o argument (%s), %s format' % (pgreport, fmt))
    thistest.fail_if(
        len(contents_of(pgout)) > 0,
        'expect empty stdout (directed to %s), %s format' % (pgout, fmt))


for spec in [{'level': 'branch', 'fmt': 'asm', 'key': 'level'},
             {'level': 'stmt', 'fmt': 'report', 'key': 'VIOLATIONS'}]:
    check_output(spec)
thistest.result()
