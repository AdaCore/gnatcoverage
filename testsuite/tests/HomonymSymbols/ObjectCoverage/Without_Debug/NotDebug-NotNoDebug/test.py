"""
Check that "f" symbols, one with (in "test_1.c") and the other without debug
information (in "test_2.c") are not consolidated whereas they are in the same
compile unit.
"""

from OCOV.tc import TestCase
from SUITE.context import thistest


# We are trying to consolidate the traces of these test drivers. Each test
# driver has its own project, since it can use different compilation options
# for test controllers:
test_drivers = {'test_1': {'not.c': ['-g0', '-O0']},
                'test_2': {'not.c': ['-g',  '-O0']}}
# Here, the "not.c" compile unit has debug info only in one test driver.

coverage_expectations = {
    # Both not.c:f() are partially covered (each one covers a complementary
    # part, so they would be fully covered if consolidated).
    'f': {'-': 0, '!': 2, '+': 0},
}

TestCase(test_drivers, coverage_expectations,
         extra_sourcedirs=['../../../../src']).run()
thistest.result()
