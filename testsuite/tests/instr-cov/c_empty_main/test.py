"""
Regression testcase: checks that gnatcov does not crash when the main that is
to be instrumented for the dump instruction is an empty function (i.e. it
has no statement to insert an instruction before).
"""

from SCOV.tc import TestCase, CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()

thistest.result()
