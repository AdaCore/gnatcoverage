"""
Check that statement coverage of ELSIF statements works as intended
when the decision is of a type derived from Standard.Boolean
(and not a subtype of Standard.Boolean). Gnatcov used to produce
illegal code in this case.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase().run()
thistest.result()
