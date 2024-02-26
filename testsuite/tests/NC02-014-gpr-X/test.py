"""
Exercise the use of -X on the gnatcov command line to control project files
operations, with -X appearing either before or after -P.
"""

import os.path
import shutil

from e3.fs import ls

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


# We use a scenario variable to select the output directory for coverage
# reports.
extra = """
    type Report_Type is ("f", "g", "all");
    This_Report : Report_Type := external ("THIS_REPORT", "all");
    package Coverage is
       for Switches ("coverage") use ("--output-dir", "output-" & This_Report);
    end Coverage;
"""

report_types = ["f", "g"]


def output_dir(what):
    return "output-{}".format(what)


# Build and run once, in a dedicated temp directory
wd = Wdir("tmp_")

base_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=[".."], extra=extra)
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)


# Now perform analysis variants


def trycov(this_report, where):
    # Clear output directories, to notice what is created that should not
    for what in report_types:
        d = output_dir(what)
        if os.path.isdir(d):
            shutil.rmtree(d)

    # THIS_REPORT conveys what value to pass as -XTHIS_REPORT.
    # WHERE conveys whether -X should be placed 'before' or 'after' -P.
    args = list(base_args)
    arg_i = len(args) if where == "after" else 1
    args.insert(arg_i, "-XTHIS_REPORT={}".format(this_report))
    xcov(args)

    this_output_dir = output_dir(this_report)
    expected_report = os.path.join(this_output_dir, "main.adb.xcov")
    reports = ls(os.path.join(output_dir("*"), "*.xcov"))

    thistest.fail_if(
        len(reports) > 1 or expected_report not in reports,
        "expected a single {}, got {}".format(expected_report, reports),
    )


for what in report_types:
    for where in ("before", "after"):
        trycov(this_report=what, where=where)

thistest.result()
