"""
Regression test: Ensure decision coverage is not affected by case-expression
in if conditions.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase(gexpr_lvl=True).run()
thistest.result()
