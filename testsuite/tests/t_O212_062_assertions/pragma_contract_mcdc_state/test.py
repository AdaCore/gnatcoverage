# Regression test: the local MC/DC state for a decision in a subprogram body
# used to be declared ahead of a contract pragma in the same declarative part,
# which the compiler rejects: nothing but pragmas may precede such a pragma.
#
# The state for an assertion pragma, on the other hand, must be declared
# before it, even when that pragma follows the contract pragmas.

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc, assert_lvl="atcc").run()
thistest.result()
