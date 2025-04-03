"""
Make sure that the instrumentation of calls of a struct member function pointer
is correctly instrumented in C.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest

TestCase(category=CAT.stmt, fun_call_lvl=True).run(
    covcontrol=CovControl(dump_trigger="manual")
)
thistest.result()
