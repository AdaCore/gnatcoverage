"""
Check coverage reports with a branch exemption in the "ELSE" part of an Ada IF
statement.
"""

import re

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


warnings = [
    "*** pkg.adb:9:13: warning: Ignoring exemption at pkg.adb:9:13 for outcome"
    " TRUE as it is already covered by the exemption region starting at"
    " pkg.adb:7:10",
    "*** pkg.adb:10:13: warning: Ignoring annotation EXEMPT_ON that intersects"
    " with region starting at pkg.adb:7:10",
]

TestCase(
    category=CAT.decision,
    tolerate_cov_messages="|".join(re.escape(w) for w in warnings),
).run()
thistest.result()
