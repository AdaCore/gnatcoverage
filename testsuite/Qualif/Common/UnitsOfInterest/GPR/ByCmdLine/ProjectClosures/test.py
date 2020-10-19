from SUITE.context import thistest
from test_support import check

# -Pgen, recursive,
# expect intops, boolops & counters reports

check(
    root_project='gen.gpr',
    recurse=True,
    xreports=['intops', 'boolops', 'counters'])

# -Pgen --projects=intops.gpr, recursive,
# expect intops & counters reports

check(
    root_project='gen.gpr',
    projects=['intops'],
    recurse=True,
    xreports=['intops', 'counters'])

# -Pgen --projects=boolops.gpr, recursive,
# expect boolops & counters reports

check(
    root_project='gen.gpr',
    projects=['boolops'],
    recurse=True,
    xreports=['boolops', 'counters'])

thistest.result()
