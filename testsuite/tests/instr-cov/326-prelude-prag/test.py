"""
Test that the instrumentation process correctly takes into account Annotate
pragmas placed in the prelude of a unit. They used to be silently ignored.
"""

from SUITE.context import thistest
from SCOV.tctl import CAT
from SCOV.tc import TestCase

TestCase(category=CAT.stmt).run()

thistest.result()
