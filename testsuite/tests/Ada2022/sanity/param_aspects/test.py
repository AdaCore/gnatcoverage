"""
Sanity test checking that there are no crashes in gnatcov when processing
sources containing aspects attached to subprograms formal parameters.

As the only aspect that can be attached to a formal subprogram parameter
supported by GNAT is Unreferenced, which accepts no argument, we currently
have no way of testing a hypothetical instrumentation of this kind of aspect.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.stmt).run()
thistest.result()
