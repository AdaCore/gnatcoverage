"""
Regression test: Check the "undetermined coverage" for guarded expressions.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase(
    gexpr_lvl=True,
    tolerate_messages="warning: Guarded Expression coverage is not"
    " available before Ada 2022",
).run()
thistest.result()
