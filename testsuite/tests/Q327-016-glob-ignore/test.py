"""
Check that the --excluded-source-file option works as expected.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


src_dir = os.path.abspath("src")
ignore_list = os.path.abspath("ignore.list")

wd = Wdir("tmp_")


# Produce a report with specific patterns to ignore some source files
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(["main.adb"], srcdirs=src_dir)),
    covlevel="stmt",
    mains=["main"],
    ignored_source_files=["@" + ignore_list],
    extra_coverage_args=["-a", "xcov", "--output-dir=."],
    tolerate_coverage_messages="no SID file found for unit",
)

# Check we have the expected reports
check_xcov_reports(
    ".",
    {
        "main.adb.xcov": {"+": {6, 7}},
        "pkg_a.adb.xcov": {"+": {6}},
        "pkg_a.ads.xcov": {},
        "pkg_b.ads.xcov": {},
        "pkg_c.ads.xcov": {},
    },
)
thistest.result()
