"""
Check that "f" symbols, both without debug information, in "test_1.c" and
"test_2.c" are not consolidated (they are not in the same compile unit,
anyway).
"""

from OCOV.tc import TestCase
from SUITE.context import thistest


# We are trying to consolidate the traces of these test drivers. Each test
# driver has its own project, since it can use different compilation options
# for test controllers:
test_drivers = {
    "test_1": {"not.c": ["-g0", "-O0"]},
    "test_2": {"fact.c": ["-g0", "-O0"]},
}
# Here, no compile unit has specific compilation flag: no one has debug info.

coverage_expectations = {
    # not.c:f() is fully covered
    # fact.c:f() is partially covered
    "f": {"-": 0, "!": 1, "+": 1},
}

TestCase(
    test_drivers, coverage_expectations, extra_sourcedirs=["../../../../src"]
).run()
thistest.result()
