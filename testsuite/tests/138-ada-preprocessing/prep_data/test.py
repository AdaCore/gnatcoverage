"""
Exhaustively check that the features of preprocessing configuration are
correctly used.
"""

from dataclasses import dataclass
import os
import os.path

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


@dataclass
class BaseTestcase:
    label: str


@dataclass
class InstrErrorTestcase(BaseTestcase):
    """Testcase that expects an instrumentation error."""
    expected_msg: str
    """Regexp for the expected instrumentation error message."""


@dataclass
class ReportTestcase(BaseTestcase):
    """Testcase that expects successful instrumentation+build+coverage."""

    expected_cov: dict[str, set[int]]
    """Expected coverage report for "pkg.adb"."""


# Run a full instrument+build+coverage cycle on test projects that have their
# own "pkg.adb" implementation and "prep.txt" preprocesor configuration data.
# Both exercise a particular preprocessor configuration feature: the
# compilation would fail with a misimplementation for that feature.
for t in [
    ReportTestcase("def_empty", {"+": {7}}),
    ReportTestcase("def_string", {"+": {6, 8, 9}}),
    ReportTestcase("def_symbol", {"+": {6, 8, 9}}),
    ReportTestcase("undef_is_false", {"+": {10, 14}}),
    InstrErrorTestcase(
        "no_undef_is_false",
        ".*gnatcov.*: instrumentation failed for .*pkg\\.adb"
        "\n.*gnatcov.*: please make sure the original project can be"
        " compiled"
        '\n.*gnatcov.*: pkg\\.adb:7:5: unknown symbol "\\$X"',
    ),
    ReportTestcase("default_config", {"+": {7}}),
    ReportTestcase("file_config", {"+": {7}}),
]:
    thistest.log(f"== {t.label} ==")
    tmp = Wdir(f"tmp_{t.label}")

    # Avoid "creating output path" info messages
    os.mkdir("obj")

    gprsw = GPRswitches(
        root_project=gprfor(
            mains=["main.adb"],
            srcdirs=["..", f"../{t.label}"],
            compiler_extra=(
                'for Default_Switches ("Ada")'
                ' use ("-gnatep=" & Project\'Project_Dir'
                f' & "/../{t.label}/prep.txt", '
                f' "-I" & Project\'Project_Dir & "../{t.label}");'
            ),
        ),
        units=["pkg"],
    )

    if isinstance(t, InstrErrorTestcase):
        # We expect an instrumentation error: check its message
        log_filename = f"{t.label}-out.txt"
        p = xcov_instrument(
            gprsw=gprsw,
            covlevel="stmt",
            register_failure=False,
            out=log_filename,
            extra_args=["--save-temps"],
        )
        thistest.fail_if(
            p.status == 0,
            "'gnatcov instrument' is supposed to fail",
        )
        output = contents_of(log_filename)
        thistest.fail_if_no_match(
            "'gnatcov instrument' output",
            t.expected_msg,
            contents_of(log_filename).strip(),
        )

    else:
        assert isinstance(t, ReportTestcase)
        # If we expect a coverage report, compute it and check its contents

        build_run_and_coverage(
            gprsw=gprsw,
            covlevel="stmt",
            mains=["main"],
            extra_args=["--save-temps"],
            extra_coverage_args=["-axcov", "--output-dir=xcov"],
            trace_mode="src",
        )
        check_xcov_reports(
            "xcov",
            {"pkg.adb.xcov": t.expected_cov, "pkg.ads.xcov": {}},
            discard_empty=False,
        )

    tmp.to_homedir()
    thistest.log("")

thistest.result()
