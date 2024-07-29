"""
Check the warning message that "gnatcov coverage" emits when a checkpoint is
provided but no coverage level is, and check that we got the expected
statement coverage results.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov, thistest

tmp = Wdir("tmp_")

# Create a checkpoint with --level=stmt+decision
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=[".."]),
    ),
    extra_coverage_args=["--save-checkpoint=my_checkpoint.ckpt"],
    covlevel="stmt+mcdc",
    mains=["main"],
    trace_mode="src",
)

# Try to load the checkpoint without specifying the coverage level and
# check the coverage results expecting stmt level coverage.
log_file = "gnatcov.out"
p = xcov(
    ["coverage", "--annotate=xcov", "--checkpoint=my_checkpoint.ckpt"],
    out=log_file,
    tolerate_messages="Coverage level not specified on the command line.*",
)


thistest.fail_if_no_match(
    what="Missing warning in the gnatcov coverage output",
    regexp=r"warning: Coverage level not specified on the command line"
    r' or in the project file \(--level=.*\), defaulting to "stmt"\.',
    actual=contents_of(log_file),
)

check_xcov_reports(".", {"main.adb.xcov": {"+": {2, 5, 6}}})

thistest.result()
