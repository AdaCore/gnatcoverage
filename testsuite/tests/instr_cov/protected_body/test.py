"""
Check that degenerate subprograms are correctly instrumented in protected
bodies
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.mcdc).run()
thistest.result()
