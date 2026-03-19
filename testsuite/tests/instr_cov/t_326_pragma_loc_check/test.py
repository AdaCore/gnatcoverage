"""
Test that buffer dump/reset annotation pragmas located in a location other than
a statement sequence are reported and ignored.
"""

import os

from SUITE.context import thistest
from SUITE.tutils import contents_of
from SCOV.tctl import CAT, CovControl
from SCOV.tc import TestCase

warning_regexp = (
    r"(\n?\*\*\* pkg\.adb:.*: warning: Incorrect placement"
    r" for a buffer dump/reset annotation, the pragma should"
    r" be placed in a statement sequence\.)"
)

TestCase(category=CAT.stmt, tolerate_messages=warning_regexp).run(
    CovControl(dump_trigger="manual")
)

# Actually check that we got the expected messages
thistest.fail_if_no_match(
    "missing or unexpected messaged from gnatcov instrument",
    regexp=warning_regexp + "+",
    actual=contents_of(os.path.join("s_pkg", "xinstr.out")),
)

thistest.result()
