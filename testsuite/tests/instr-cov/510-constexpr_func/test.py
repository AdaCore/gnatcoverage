"""
Check that gnatcov correctly reports constexpr functions as not instrumented
"""

from os.path import join

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest
from SUITE.tutils import contents_of

TestCase(
    category=CAT.stmt,
    tolerate_messages="cannot instrument constexpr",
).run()

thistest.fail_if_no_match(
    what="Unexpected 'gnatcov instrument' message",
    regexp=r".*test_main.cpp:\d+:\d+: warning: gnatcov limitation: cannot"
    " instrument constexpr function, it will be reported as undetermined"
    " coverage",
    actual=contents_of(join("s_main/xinstr.out")),
)

thistest.result()
