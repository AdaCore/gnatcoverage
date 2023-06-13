# -*- coding: utf-8 -*-

"""
Check that the order of symbols in GNATcov's assembly coverage report is
deterministic (sorted, even).
"""

import re
from SUITE.control import target_info
from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import (exepath_to, gprfor, gprbuild, thistest,
                          tracename_for, xcov, xrun)


tmp = Wdir('tmp_')

symbols = [target_info().to_platform_specific_symbol(s)
           for s in ['pkg__bar', 'pkg__foo']]

routines = 'routines.txt'
report = 'coverage-report.txt'
with open(routines, 'w') as f:
    for s in symbols:
        f.write(s + '\n')

prj = gprfor('p.adb', srcdirs='..')
gprbuild(prj)

xrun(exepath_to('p'))
xcov(['coverage', '-cinsn', '-aasm', '-o', report,
      '--routines=@routines.txt', tracename_for('p')])

report_symbols = sorted(
    line.split()[0] for line in lines_of(report)
    if re.match('^[a-z_]* [+!-]: [0-9a-f]+-[0-9a-f]+', line.rstrip())
)


def fmt_list(items):
    return '\n'.join('   ' + line for line in items)


thistest.fail_if(
    symbols != report_symbols,
    'Unexpected sequence of symbols, expected:\n'
    '{}\n'
    'but got:\n'
    '{}'.format(fmt_list(symbols), fmt_list(report_symbols))
)

thistest.result()
