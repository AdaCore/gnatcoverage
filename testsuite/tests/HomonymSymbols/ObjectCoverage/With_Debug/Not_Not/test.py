"""
Check that "f" symbols, both with debug information and from the same
compilation unit, are consolidated.
"""

from OCOV.tc import TestCase
from SUITE.context import thistest

switches = ["-O0", "-g"]

# Test expectations
test_drivers = {"test_1": {"not.c": switches}, "test_2": {"not.c": switches}}
coverage_expectations = {
    # Both not.c:f() are partially covered (each one covers a complementary
    # part), so there is one fully covered routine after consolidation.
    "f": {"-": 0, "!": 0, "+": 1},
}

TestCase(
    test_drivers, coverage_expectations, extra_sourcedirs=["../../../../src"]
).run()
thistest.result()
