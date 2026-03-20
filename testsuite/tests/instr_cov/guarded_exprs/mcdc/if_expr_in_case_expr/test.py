"""
Regression test: Ensure that case-expression instrumentation is not affected
by nesting in if-expressions.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase(gexpr_lvl=True).run()
thistest.result()
