"""
Check that the instrumentation in MC/DC mode of an expression function that
contains defining identifiers does not crash (it used to).
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
