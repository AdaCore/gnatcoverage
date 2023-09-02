"""
Test correct support for generalized array aggregates, including iterated
component associations.
"""


from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
thistest.result()
