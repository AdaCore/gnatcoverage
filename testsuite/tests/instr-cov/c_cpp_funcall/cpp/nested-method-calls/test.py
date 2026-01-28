"""
This test ensures that method call instrumentation behaves correctly and
produces the expected coverage.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest

TestCase(category=CAT.stmt, fun_call_lvl=True).run(
    covcontrol=CovControl(dump_trigger="manual")
)
thistest.result()
