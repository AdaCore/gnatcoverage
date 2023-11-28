"""
Test correct handling and instrumentation of container aggregates, and decisions
nested within.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


# GNAT hangs on instrumented code (eng/toolchain/gnat#502): to avoid
# systematically hitting timeouts (wasting precious minutes in a testsuite
# run), abort the testcase early until this is fixed.
thistest.stop(RuntimeError)
TestCase(category=CAT.decision).run()
thistest.result()
