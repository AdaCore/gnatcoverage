"""
This is a regression test that checks that gnatcov produces valid code even
if the standard package is hidden by a user-defined enumeration member.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.mcdc).run()
thistest.result()
