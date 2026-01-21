"""
Check that gnatcov correctly reports constexpr functions as not instrumented.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest
from SUITE.tutils import contents_of

TestCase(
    category=CAT.stmt,
    tolerate_messages="cannot instrument constexpr",
).run()

thistest.fail_if_not_equal(
    what="Unexpected 'gnatcov instrument' message",
    expected="??? test_main.cpp:4:1: gnatcov limitation: cannot"
    " instrument constexpr function, it will be reported as undetermined"
    " coverage",
    actual=contents_of("s_main/xinstr.out").strip(),
)

thistest.result()
