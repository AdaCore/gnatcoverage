"""
Regression testcase specific to the Ada instrumenter (material and bug report
come from CS0038132): check that gnatcov correctly instruments a decision that
involves a derived boolean type.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.decision).run()
thistest.result()
