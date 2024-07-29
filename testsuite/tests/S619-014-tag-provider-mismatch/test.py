"""
Check that "gnatcov coverage" complains when loading a checkpoint created with
a different tag provider, i.e. asking for inconsistent separate coverage. Also
check that the coverage report correctly disregards coverage data from the
loaded checkpoint.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


wd = Wdir("tmp_")

# First, create a checkpoint using the default tag provider
ckpt = "c.ckpt"
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(["main.adb"], srcdirs="..")),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["--save-checkpoint", ckpt],
)

# Now try to load it while asking for the non-default tag provider. Don't pass
# the trace file, as we are interested only in checkpoint processing.
xcov(
    [
        "coverage",
        "-cstmt+decision",
        "-axcov",
        "-Sinstance",
        "-C",
        ckpt,
        "--output-dir=.",
    ],
    out="consolidate.log",
    tolerate_messages=".",
)

expected = (
    "warning: -S is deprecated. This option will be removed in release 26."
    "\nwarning: cannot merge coverage information from {} as it is separated"
    " by default".format(ckpt)
)
actual = contents_of("consolidate.log").strip()
thistest.fail_if(
    expected != actual,
    'Unexpected output for "gnatcov coverage". Expected:\n'
    "{}\n"
    "but got:\n"
    "{}".format(indent(expected), indent(actual)),
)

expected_cov = {
    "main.adb.xcov": {},
    "generic_hello.adb.xcov": {},
}
if thistest.options.trace_mode == "src":
    expected_cov["generic_hello.ads.xcov"] = {}
check_xcov_reports(".", expected_cov, discard_empty=False)

thistest.result()
