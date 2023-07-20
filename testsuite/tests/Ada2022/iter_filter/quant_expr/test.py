"""
Test the correct handling and instrumentation of iterator filters, present
within a quantified expression.

The naming convention of the test drivers is
test_<filter valuations>_<predicate valuations>.adb
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
thistest.result()
