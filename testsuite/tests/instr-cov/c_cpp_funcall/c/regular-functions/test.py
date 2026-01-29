"""
Check that the Fun_Call coverage instrumentation works for simple C functions.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt, fun_call_lvl=True).run()
thistest.result()
