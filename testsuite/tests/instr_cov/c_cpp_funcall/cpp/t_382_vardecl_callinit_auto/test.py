"""
Checks the case of instrumentation of variable declarations that have an `auto`
type and use the "CallInit" syntax.

CURRENTLY, these are not instrumented and reported as a limitation.

Example: Foo foo(...);

Foo bar(foo); // We check call instrumentation on this
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

WARNING = (
    "functional constructor call with `auto` type will not be instrumented"
)

TestCase(category=CAT.stmt, fun_call_lvl=True, tolerate_messages=WARNING).run()
thistest.result()
