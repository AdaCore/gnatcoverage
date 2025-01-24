"""
Check that the C++ instrumenter does not consider `using` clauses
as statements.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()
thistest.result()
