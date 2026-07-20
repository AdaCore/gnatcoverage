"""
Check that decision outcome exemptions specified with a decision offset are
resolved as expected even in the presence of declare expressions and nested
decisions in them.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
thistest.result()
