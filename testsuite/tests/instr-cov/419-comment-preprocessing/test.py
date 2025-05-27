"""
Test that gnatcov does not crash when instrumenting a source that cannot be
preprocessed while preserving comments, and instead emits a warning stating
that annotations are ignored.
"""

import os

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest
from SUITE.cutils import contents_of

wd = "s_pkg"

warning_pat = (
    r"warning: Could not preserve comments while pre-processing .*test_pkg\.c"
    r", annotations in comments within this file or included headers will not"
    r" be taken into account"
)

TestCase(
    category=CAT.stmt,
    tolerate_messages=warning_pat,
).run()

# Actually check that the warning got emitted
thistest.fail_if_no_match(
    what="missing warning in the output of 'gnatcov instrument'",
    regexp=warning_pat,
    actual=contents_of(os.path.join(wd, "xinstr.out")),
)

thistest.result()
