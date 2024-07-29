"""
Verify that gnatcov cleans up existing SID files during instrumentation.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import contents_of, gprfor

Wdir("tmp_")

prj = gprfor(mains=["main.adb"], srcdirs=[".."])

expected_cov = {
    "main.adb.xcov": {"+": {9, 8}},
    "pkg.adb.xcov": {"+": {4}},
    "pkg.ads.xcov": {},
    "pkh.adb.xcov": {"+": {4}},
    "pkh.ads.xcov": {},
}

# Create a first report using all units
thistest.log("=== All units test ===")
output_full = "all_units"
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir", output_full],
)
check_xcov_reports(output_full, expected_cov)

thistest.log("=== Without Pkh unit test ===")
# Now same but without pkg in the units of interest, at instrumentation time
# Only change the output dir, and not the object dir so that if the pkh.sid
# file remains after instrumentation, it will be picked up and generate a
# report with violations.
#
# In this case, we expect pkh.sid to be deleted, so no report created for this
# unit (and gnatcov coverage should complain that there is no SID for this
# specific unit).

output_pkg = "only_pkg"

# Pass the units argument to gnatcov instrument only to ensure the coverage
# command will attempt to pick up all sid files.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--units", "main", "--units", "pkg"],
    extra_coverage_args=["-axcov", "--output-dir", output_pkg],
    tolerate_coverage_messages="no SID file found for unit pkh",
)
expected_cov.pop("pkh.adb.xcov")
expected_cov.pop("pkh.ads.xcov")
check_xcov_reports(output_pkg, expected_cov)

# Verify that gnatcov complained that there was no SID file found for Pkh
thistest.fail_if_not_equal(
    what="Unexpected 'gnatcov coverage' output",
    expected="warning: no SID file found for unit pkh",
    actual=contents_of("coverage.log").strip(),
)

thistest.result()
