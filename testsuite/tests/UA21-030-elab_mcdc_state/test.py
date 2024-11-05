"""
Test that decisions in elaboration code (both declarations and package body
statments) are proprely instrumented for MCDC.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest
from SCOV.tctl import CAT

TestCase(category=CAT.mcdc).run()
thistest.result()
