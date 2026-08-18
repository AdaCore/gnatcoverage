"""
Check that gnatcov does not instrument C constant-folded expressions.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(
    category=CAT.mcdc,
).run()

TestCase(
    category=CAT.decision,
).run()

thistest.result()
