"""
Check that gnatcov does not produce illegal Ada code for function call coverage
on pre-Ada 2012 codebases.
"""

import re

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest
from SUITE.cutils import contents_of


warning = (
    "??? pkg.adb:21:36: gnatcov limitation: cannot instrument calls"
    " before Ada 2012"
)
TestCase(
    category=CAT.stmt,
    fun_call_lvl=True,
    tolerate_messages=re.escape(warning),
).run()

# Check that gnatcov output contains the expected error messages
thistest.fail_if_not_equal(
    "gnatcov instrument output",
    warning,
    contents_of("ffc_pkg/xinstr.out").strip(),
)

thistest.result()
