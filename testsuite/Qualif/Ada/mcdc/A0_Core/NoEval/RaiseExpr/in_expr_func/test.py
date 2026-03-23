"""
Test the correct handling and instrumentation of raise expressions,
within a return statement, as part of a decision.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest


TestCase().run()
thistest.result()
