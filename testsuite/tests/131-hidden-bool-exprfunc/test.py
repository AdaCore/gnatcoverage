"""
Regression test: when the "Boolean" standard entity is hidden by a user-defined
entity, the stmt instrumentation of expression functions used to generate
invalid code.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.stmt).run()
thistest.result()
