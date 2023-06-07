"""
Check that we properly instrument recursive expression functions
without aspects but with a previous declaration.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
