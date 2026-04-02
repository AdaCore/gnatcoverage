"""
Check that the Type_Invariant'Class is correctly instrumented in ATC/ATCC mode
and that is is properly covered when checked through a child of the tagged
type.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

# ATC
TestCase(category=CAT.stmt, assert_lvl="atc").run()
TestCase(category=CAT.decision, assert_lvl="atc").run()
TestCase(category=CAT.mcdc, assert_lvl="atc").run()

# ATCC
TestCase(category=CAT.stmt, assert_lvl="atcc").run()
TestCase(category=CAT.decision, assert_lvl="atcc").run()
TestCase(category=CAT.mcdc, assert_lvl="atcc").run()

thistest.result()
