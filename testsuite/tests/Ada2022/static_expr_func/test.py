"""
Test checking that gnatcov correctly instruments static expression functions
(annotated with the static aspect).
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()
thistest.result()
