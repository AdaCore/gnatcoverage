"""
Test the correct handling and instrumentation of raise expressions,
within a return statement, as part of a decision.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
