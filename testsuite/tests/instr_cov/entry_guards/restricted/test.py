"""
Check that protected bodies' entry guard decisions are not instrumented in the
presence of Pure_Barriers restrictions.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


for covlevel, slug, expected_coverage in [
    ("stmt", "st", {"+": {12, 13, 24}}),
    ("stmt+decision", "dc", {"+": {12, 13, 24}, "?": {21}}),
    ("stmt+mcdc", "mc", {"+": {12, 13, 24}, "?": {21}}),
]:
    thistest.log(f"== {covlevel} ==")
    tmp = Wdir(f"tmp_{slug}")

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=["../src"], mains=["test_true.adb"]),
            units=["pkg"],
        ),
        covlevel=covlevel,
        mains=["test_true"],
        extra_coverage_args=["-axcov", "--output-dir=."],
    )
    check_xcov_reports(
        ".", {"pkg.adb.xcov": expected_coverage, "pkg.ads.xcov": {}}
    )

    tmp.to_homedir()

thistest.result()
