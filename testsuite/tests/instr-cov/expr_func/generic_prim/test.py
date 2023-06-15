"""
Check that we properly instrument expression functions that are primitives,
declared in a generic package without triggering any freezing bug.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.stmt).run()
thistest.result()
