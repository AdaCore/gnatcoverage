"""
Test correct support for @, the LHS reference shortcut:
 - It is correctly instrumented when it is in an expression operand of an
   initialization statement, without control flow.
 - It is correctly instrumented when featured as the decision of an if
   expression.
 - It is correctly instrumented when featured as a boolean operand of an if
   expression.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
