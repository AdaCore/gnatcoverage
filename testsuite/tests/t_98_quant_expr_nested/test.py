"""
Check that decisions nested in a quantified expression but not in the predicate
are correctly instrumented. These used to be silently ignored by gnatcov.

The naming convention for the test drivers is as follows:
test_<if_expr_valuations>_<predicate_valuations>.adb
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
thistest.result()
