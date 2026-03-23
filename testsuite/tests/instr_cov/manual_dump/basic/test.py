"""
This test checks that we don't break the GNATcov_RTS API for manual trace
dumps.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(
            prjid="main",
            srcdirs=[".."],
            mains=["main.adb"],
            langs=["Ada"],
            # Depending on the build mode, use a different implementation of
            # the Covdump procedure. This allows the regular build to work (no
            # dependency on GNATcov_RTS nor coverage buffers) and at the same
            # time, allows the coverage build to do the coverage dump.
            scenario_extra="""
                type Boolean is ("False", "True");
                Cov_Build : Boolean := external ("COV_BUILD", "False");
            """,
            extra="""
                package Naming is
                   case Cov_Build is
                      when "False" =>
                         for Body ("covdump") use "covdump__nocov.adb";
                      when "True" =>
                         for Body ("covdump") use "covdump__cov.adb";
                   end case;
                end Naming;
            """,
        ),
        units=["main"],
    ),
    covlevel="stmt",
    mains=["main"],
    extra_gprbuild_args=["-XCOV_BUILD=True"],
    extra_coverage_args=["--annotate=xcov", "-XCOV_BUILD=True"],
    tolerate_instrument_messages="warning: no indication for dump location was"
    " found, this might be caused by a"
    " misspelling in the expected pragma"
    " statement.",
    dump_trigger="manual",
    manual_prj_name="main",
)
check_xcov_reports("obj", {"main.adb.xcov": {"+": {5}}})

thistest.result()
