"""
Check that an exemption that targets a complex decision in an expression
context (thus disregarded as a decision for --level=stmt+decision) does not
affect another decision that comes just after.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
thistest.result()
