from SUITE.context import thistest
from test_support import check

# -Pgen --projects=intops.gpr, not recursive,
# expect intops related reports only

check(
    root_project='gen.gpr',
    projects=['intops'],
    recurse=False,
    xreports=['intops'])

# -Pgen --projects=boolops.gpr, not recursive,
# expect boolops related reports only

check(
    root_project='gen.gpr',
    projects=['boolops'],
    recurse=False,
    xreports=['boolops'])

# -Pgen --projects=boolops.gpr --projects=intops.gpr, not recursive
# expect boolops & intops related reports

check(
    root_project='gen.gpr',
    projects=['boolops', 'intops'],
    recurse=False,
    xreports=['boolops', 'intops'])

# -Pgen --projects=boolops.gpr --projects=counters.gpr, not recursive
# expect boolops & counters related reports

check(
    root_project='gen.gpr',
    projects=['boolops', 'counters'],
    recurse=False,
    xreports=['boolops', 'counters'])

thistest.result()
