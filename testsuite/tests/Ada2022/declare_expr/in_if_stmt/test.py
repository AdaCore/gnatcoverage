"""
Test the correct handling and instrumentation of declare expressions,
within an if statement, as part of a decision.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
