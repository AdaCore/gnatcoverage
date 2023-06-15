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
test_drivers = {'test_1': {'not.c': ['-g',  '-O0']},
                'test_2': {'not.c': ['-g',  '-O0']},
                'test_3': {'not.c': ['-g0', '-O0']}}
# Here, the "not.c" compile unit has debug info except in one test driver (the
# last one).

coverage_expectations = {
    # - In test_1, not.c:f() is half-covered
    # - The other half is covered in test_2
    # - Some part is covered in test_3
    #
    # test_1 and test_2 are consolidated, so there must be one f() that is
    # fully covered. One other is partially covered for test_3 that do not
    # consolidate.
    'f': {'-': 0, '!': 1, '+': 1},
}

TestCase(test_drivers, coverage_expectations,
         extra_sourcedirs=['../../../../src']).run()
thistest.result()
