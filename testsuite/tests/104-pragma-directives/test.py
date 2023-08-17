"""
Check that the instrumentation of a test with pragma preprocessing directives
works fine.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()
thistest.result()
