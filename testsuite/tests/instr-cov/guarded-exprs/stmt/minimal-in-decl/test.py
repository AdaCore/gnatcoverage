"""
Regression test: Ensure that case-expressions in declaration sections are
correctly instrumented.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase(gexpr_lvl=True).run()
thistest.result()
