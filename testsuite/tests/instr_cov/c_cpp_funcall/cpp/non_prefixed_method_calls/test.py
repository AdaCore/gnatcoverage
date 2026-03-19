"""
This test makes sure that the instrumentation of non-prefixed method calls
works correctly.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt, fun_call_lvl=True).run()
thistest.result()
