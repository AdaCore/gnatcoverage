"""
Check that --units override Coverage attributes in all projects.
"""

from SUITE.context import thistest
from test_support import check

# boolops.gpr features a Units attribute in Coverage project,
# listing all the units of the project

# Without --units, on closure -> everything

check(
    root_project='gen.gpr',
    recurse=True,
    xreports=['intops', 'boolops', 'counters'])

# --units=boolops.andthen in closure, andthen and parent
# spec reports only, despite the attribute in boolops.gpr

check(
    root_project='gen.gpr',
    recurse=True,
    units=['boolops.andthen'],
    xreports=['boolops.ads', 'boolops-andthen.adb'])

# --units=counters in closure, counters reports only
# despite the attribute in boolops.gpr

check(
    root_project='gen.gpr',
    recurse=True,
    units=['counters'],
    xreports=['counters.ads', 'counters.adb'])

thistest.result()
