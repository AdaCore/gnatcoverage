"""
Check that gnatcov (instrument/run/coverage) successfully handles very big
decisions in MC/DC. For such decisions with an overly large BDD path count, the
tool is simply expected not to instrument the decision and thus report
undetermined MC/DC coverage for it.
"""

import re

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

gpr = gprfor(mains=["test_eval.adb"], srcdirs=["../src"], langs=["Ada", "C"])

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    extra_coverage_args=["--annotate=xcov"],
    covlevel="stmt+mcdc",
    mains=["test_eval"],
    trace_mode="src",
    tolerate_instrument_messages=\
      "Number of distinct paths .* exceeds the limit",
)

check_xcov_reports(
    "*.xcov",
    expected_cov={
        "test_eval.adb.xcov": {"+": {11, 13, 19, 20}},
        "testconditions.adb.xcov": {"?": {17}, "+": {22, 23, 24, 25, 26, 27}},
        "compute.c.xcov": {"?": {4}, "+": {5, 6}},
    },
    cwd="obj",
)


def warning_re_for(filename, sloc):
    """
    Return a regexp for the warning expected for the given file name and source
    location.
    """
    return (
        r"\*\*\* {filename}:{sloc}: warning: Number of distinct paths in the"
        r" decision exceeds the limit \(\d+\)\. MC/DC coverage for this"
        r" decision will be left undetermined in coverage reports\. Use option"
        r" --path-count-limit to adjust the limit if the default value is too"
        r" low\."
    ).format(filename=re.escape(filename), sloc=re.escape(sloc))


# The order in which sources are instrumented is not specified, so sort lines
# in the output of "gnatcov instrument" to get deterministic test execution.
log = "\n".join(sorted(contents_of("instrument.log").splitlines()))

thistest.fail_if_no_match(
    what="Unexpected/missing warnings for MC/DC path limit",
    regexp="^" + "\n".join([
        warning_re_for("compute.c", "4:11"),
        warning_re_for("testconditions.adb", "17:9"),
    ]),
    actual=log,
)

thistest.result()
