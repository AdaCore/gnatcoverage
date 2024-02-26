import os.path
import re

from SCOV.minicheck import build_and_run
from SUITE.cutils import Wdir, match
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov


src_dir = os.path.abspath(".")
wd = Wdir("tmp_")

xcov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(["foo.adb"], srcdirs=src_dir, objdir=".")
    ),
    gpr_obj_dir=".",
    covlevel="stmt",
    mains=["foo"],
    extra_coverage_args=[],
    scos=["foo"],
)


def run_checks(name, report_title, prefix):
    html_dir = "{}-html".format(name)

    args = list(xcov_args)
    if report_title is None:
        pass
    else:
        args.append("--report-title={}".format(report_title))

    xcov(args + ["-ahtml", "--output-dir={}".format(html_dir)])

    def do_match(title, dirname, filename):
        path = os.path.join(dirname, filename)
        text = "<title>{}</title>".format(title)
        thistest.fail_if(
            not match(re.escape(text), path),
            "Could not match {} in {}".format(repr(text), path),
        )

    do_match(prefix + "GNATcoverage Report", html_dir, "index.html")


run_checks("none", None, "")
run_checks("empty", "", "")
run_checks("full", "My<report", "My&lt;report - ")
thistest.result()
