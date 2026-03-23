"""
Check that instrumenting a lambda containing a decision
and that is declared in a statement containing a decision (other than nested in
the lambda) works as intended. gnatcov used to emit duplicate decision SCOs and
thus crash.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.decision).run()
thistest.result()
