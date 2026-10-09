"""
Ensure GNATcov correctly supports the `finally` code construct from Flare 0.1
in case of exceptions
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()

thistest.result()
