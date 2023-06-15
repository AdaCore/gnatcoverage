"""
Check that the coverage of nested decisions in pragma Assert works as expected.
TODO: revisit this test once we decide on how to enable instrumentation of
assertions (U528-022)
"""

from SUITE.context import thistest
from SCOV.tctl import CAT
from SCOV.tc import TestCase

TestCase(category=CAT.mcdc).run()

thistest.result()
