"""
Check that the instrumentation of ghost expression functions produces valid
code. It used to create non-ghost expression functions that could reference
ghost entities.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
TestCase(category=CAT.mcdc, instrument_ghost=True).run()
thistest.result()
