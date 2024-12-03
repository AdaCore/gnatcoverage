"""
Regression test: Ensure that case-expressions in assignation statements are
correctly instrumented.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase(gexpr_lvl=True).run()
thistest.result()
