"""
Check that gnatcov can instrument null procedures that specify a calling
convention, either through the Convention aspect or through the Export aspect.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()
thistest.result()
