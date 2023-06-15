"""
Check that gnatcov tries and fails to consolidate two "f" symbols, both with
debug information and from the same compilation unit.
"""

from OCOV.tc import TestCase
from SUITE.context import thistest


# Test expectations
test_drivers = {'test_1': {'not.c': ['-g', '-O0']},
                'test_2': {'not.c': ['-g', '-O1']}}
coverage_expectations = {
    # The two not.c:f() are compiled with different levels of optimization. In
    # object coverage, gnatcov is supposed to fail: there is no coverage
    # expectation.
    'f': {'-': 0, '!': 0, '+': 0},
}

tc = TestCase(test_drivers, coverage_expectations,
              extra_sourcedirs=['../../../src'])
thistest.fail_if(tc.run(register_failure=False),
                 '"gnatcov coverage" was supposed to fail, but it did not')
thistest.result()
