"""
This test checks that gnatcov correctly deals with static decisions. The
instrumenter used to produce code triggering constraint violations as it
made such decisions not static anymore by wrapping them in witness calls.

Also check that the inner conditions are not wrapped in witness calls, as
this too renders the expression non-static.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.decision).run()
TestCase(category=CAT.mcdc).run()

thistest.result()
