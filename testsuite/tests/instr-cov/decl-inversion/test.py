"""
Check that instrumentation of declarations preserves the initial order of the
declarations in the region.
"""

from SUITE.context import thistest
from SCOV.tctl import CAT
from SCOV.tc import TestCase

TestCase(category=CAT.mcdc).run()

thistest.result()
