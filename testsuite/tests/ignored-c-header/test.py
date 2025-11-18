"""
Check that ignoring a C header file works as expected. It used to make "gnatcov
coverage" crash.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.gprutils import GPRswitches
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["test.c"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["test"],
    extra_coverage_args=[
        "--annotate=xcov",
        # Ignore directly "twice.h"
        "--excluded-source-files=twice.h",
        # Ignore indirectly "identity.h", as its CU's origin is "identity.c".
        # It is important for test reliability to check this testcase that only
        # "identity.c" includes "identity.h".
        "--excluded-source-files=identity.c",
    ],
    tolerate_coverage_messages=".",
)

expected_coverage = {
    "test.c.xcov": {"+": {8, 9}},
    "identity.h.xcov": {"+": {4}},
}

# The following is specific to the instrumentation: the CU for "identity.h"
# depends on the "identity.c". As a result, ignoring "identity.c" implicitly
# ignores "identity.h", and gnatcov is supposed to warn for this case.
#
# Since gnatcov emits a report for all source files, we still expect a report
# for "identify.h".
#
# For binary traces, the CU for "identity.h" does not depend on "identity.c"
# (its Origin is "identity.c.gli", instead), so the above is not true.
if thistest.options.trace_mode == "src":
    cov_log = contents_of("coverage.log").strip()
    thistest.fail_if_no_match(
        "'gnatcov coverage' output",
        r"warning: gnatcov limitation: ignoring unit identity\.h from"
        r".*[\\/]identity\.c\.sid because identity\.c is ignored$",
        cov_log,
    )

    expected_coverage["identity.h.xcov"] = {}

check_xcov_reports("obj", expected_coverage)

thistest.result()
