"""
Check that the previous declaration inserted for an augmented expression
function is valid. This test is not so much about what the coverage results
we ge but rather about having the instrumenter emit valid code.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
