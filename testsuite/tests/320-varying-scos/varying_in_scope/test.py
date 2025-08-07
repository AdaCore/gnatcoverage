"""
Check that gnatcov correctly load checkpoints when a source has a version that
varies across runs, and when the varying part is in the same scope for both
versions. Check that the scope metrics are as expected by inspecting the XML
report.
"""

import os
import os.path

from lxml import etree

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../src"], mains=["main.c"]),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "-axml", "--output-dir=report"],
    trace_mode="src",
)
check_xcov_reports(
    "report/xcov",
    {
        "bar.c.xcov": {},
        "foo.c.xcov": {},
        "foo.h.xcov": {"-": {4, 7}},
        "main.c.xcov": {"+": {3}},
    },
    discard_empty=False,
)

# Check scope metrics
xml = etree.parse("report/xml/foo.h.xml")
xpath_query = './/scope_metric/obligation_stats/metric[@kind="total_obligations_of_relevance"]'

# Find all matching metrics
metrics = xml.xpath(xpath_query)

# We expect 2 statement violations for the header and the associated function
thistest.fail_if(
    int(metrics[0].get("count")) != 2 or int(metrics[1].get("count")) != 2,
    "unexpected scope metrics"
)

thistest.result()
