"""
Test that gnatcov chooses the correct defaults for the level and the
annotation format, and that it emits a warning concerning the level default
value.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


def check_log(command, log_file):
    thistest.fail_if_no_match(
        what=f"Missing warning about default level for command {command}",
        regexp=r"warning: Coverage level not specified on the command line or"
        r' in the project file \(--level=.*\), defaulting to "stmt"\.',
        actual=contents_of(log_file),
    )


tmp = Wdir("tmp_")

# 1. Check the instrument command by not specifying a coverage level to
#    build_and_run. Tolerate any warning, we'll check the log.
cov_args = build_and_run(
    GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel=None,
    mains=["main"],
    tolerate_instrument_messages="Coverage level not specified",
    extra_coverage_args=[],
)

if thistest.options.trace_mode == "src":
    check_log("instrument", "instrument.log")

# We do not check the output of gnatcov run as it was already possible
# not to specify a coverage level for that command.

# 2. Check the output of the coverage command while specifying an annotation
# format, and check that the coverage report corresponds to a stmt level.
cov_log = "coverage_no_level.log"
xcov(
    cov_args + ["-axcov"],
    out=cov_log,
    tolerate_messages="Coverage level not specified",
)

check_log("coverage", cov_log)
check_xcov_reports("obj", {"main.adb.xcov": {"+": {2, 5, 6}, "-": {8}}})

# 3. Run the coverage command without specifying an annotation format and
# check if there is a report on the standard output.

cov_log = "coverage_no_annotate.log"
xcov(cov_args + ["-cstmt"], out=cov_log)
thistest.fail_if_no_match(
    what="Missing report on the standard output without annotation format",
    regexp=r"\*\* COVERAGE REPORT \*\*",
    actual=contents_of(cov_log),
)

thistest.result()
