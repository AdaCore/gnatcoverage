"""
Regression / robustness test case: we use to implement spuriously source
coverage obligations inside lambda expressions. Check that we correctly
instrument lambda expressions in various contexts.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase().run()
thistest.result()
