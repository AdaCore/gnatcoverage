"""
Ensure GNATcov correctly supports declaration between statements
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()

thistest.result()
