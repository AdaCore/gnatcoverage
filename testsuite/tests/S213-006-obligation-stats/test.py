"""
Checks that coverage obligations spanning on multiple lines are not counted
several times in the reported obligation statistics. Checking the XML content
(which also provides the obligation statistics) is verified in
R507-027-xml-summary.
"""

import re

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov

tmp = Wdir("tmp_")

covargs = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../src"],
                            mains=["test.adb"])),
    covlevel="stmt+mcdc", mains=["test"],
    extra_coverage_args=[])

xcov(covargs + ["-axcov"])
thistest.fail_if_no_match(
    'foo.adb.xcov',
    re.compile(
        ".*\n"
        r"100% statement coverage \(2 out of 2\)\n"
        r"0% decision coverage \(0 out of 1\)\n"
        r"0% MC/DC coverage \(0 out of 4\)\n"
        ".*",
        re.DOTALL
    ),
    contents_of("obj/foo.adb.xcov")
)

thistest.result()
