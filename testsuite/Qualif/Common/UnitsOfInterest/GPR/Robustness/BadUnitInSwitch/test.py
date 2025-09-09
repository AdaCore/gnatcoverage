"""
Check that invalid units passed as --units are properly reported.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor("main.adb", srcdirs=".."),
        units=["no_such_unit", "main", "helper.say_hello"],
    ),
    covlevel="stmt",
    mains=["main"],
    tolerate_instrument_messages="no unit .* in the projects of interest",
    extra_coverage_args=["-axcov"],
    tolerate_coverage_messages="no unit .* in the projects of interest",
)

log_file = (
    "coverage.log"
    if thistest.options.trace_mode == "bin"
    else "instrument.log"
)

# Split and re-join lines to avoid spurious CR/LF diffs on Windows. Also sort
# lines, as the order in which these warnings is emitted is not deterministic.
log_lines = "\n".join(
    sorted(line.rstrip() for line in contents_of(log_file).splitlines())
).rstrip()

thistest.fail_if_not_equal(
    "gnatcov output",
    "warning: no unit helper.say_hello (from --units) in the projects of"
    " interest"
    "\nwarning: no unit no_such_unit (from --units) in the projects of"
    " interest",
    log_lines,
)

check_xcov_reports("obj", {"main.adb.xcov": {"+": {5}}})

thistest.result()
