"""
Regression test for a crash in "gnatcov instrument" when processing incomplete
task type declarations.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest


TestCase().run()
thistest.result()
