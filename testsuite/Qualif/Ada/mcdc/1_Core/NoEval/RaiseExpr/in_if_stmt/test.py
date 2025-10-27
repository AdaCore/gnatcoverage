"""
Test the correct handling and instrumentation of raise expressions,
within an expression function, as part of a decision.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest


TestCase().run()
thistest.result()
