"""
Check that "f" symbols, both with debug information but from different
compilation units, are not consolidated.
"""

from OCOV.tc import TestCase
from SUITE.context import thistest


# Test expectations.  All compile units have debug symbols.
switches = ["-O0", "-g"]
test_drivers = {
    "test_1": {"not.c": switches, "fact.c": switches},
    "test_2": {"not.c": switches, "fact.c": switches},
}
coverage_expectations = {
    # - In test_1, not.c:f() is partially covered
    # - In test_2, fact.c:f() is fully covered
    #
    # There is no consolidation, so there will be one partially covered
    # routine and one fully covered one.
    "f": {"-": 0, "!": 1, "+": 1},
}

TestCase(
    test_drivers, coverage_expectations, extra_sourcedirs=["../../../../src"]
).run()
thistest.result()
