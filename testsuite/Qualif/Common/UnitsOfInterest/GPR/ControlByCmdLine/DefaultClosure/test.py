"""
Check that the tool operates in recursive mode by default
and that --no_subprojects flips this to non-recursive.
"""

from SUITE.context import thistest
from test_support import check

# -Pgen, recursiveness unspecified
# expect intops, boolops and conters related reports

check(
    root_project='gen.gpr',
    recurse=None,
    xreports=['intops', 'boolops', 'counters'])

# -Pgen, non recursive would yield an empty set of
# units of interest, so try another level first:

# -Pgen --projects=intops, recursiveness unspecified
# expect intops and counters related reports

check(
    root_project='gen.gpr',
    projects=['intops'],
    recurse=None,
    xreports=['intops', 'counters'])

# -Pgen --projects=intops --no-subprojects
# expect intops related report only

check(
    root_project='gen.gpr',
    projects=['intops'],
    recurse=False,
    xreports=['intops'])

thistest.result()
