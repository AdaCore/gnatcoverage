"""
Check that quantified expressions are correctly instrumented in object
declaration initialization expressions. This used not to be the case.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
thistest.result()
