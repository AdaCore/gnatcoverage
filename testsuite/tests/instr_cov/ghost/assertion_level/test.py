"""
Check that gnatcov correctly instruments Ghost expression functions with an
associated assertion level.

When gnatcov instruments expression functions for MC/DC, it create a wrapping
expression function: this wrapper must have the same assertion level as the
orignial function for the code to stay compilable with the same assertion
policies.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest
from SCOV.tctl import CAT

TestCase(category=CAT.mcdc, extracargs=["-gnata"], instrument_ghost=True).run()
thistest.result()
