"""
Check that gnatcov correctly handles source code that features tagged type
declarations with the First_Controlling_Parameter aspect.

Note that the presence of this aspect does not avoids the freezing problem with
expression functions that returns the tagged type declared in the same scope.
This implies that gnatcov cannot instrument these expressions functions to
perform code coverage on them (see example.adb's F and F2 expression
functions).
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(
    category=CAT.stmt,
    tolerate_messages="cannot instrument an expression function which is a"
    " primitive of its return type",
).run()
thistest.result()
