"""
Check that gnatcov correctly rejects exemption markers intersecting SCOs.
"""

# TODO: This may need to be moved to a Robustness chapter instead when
# proper qualification tests are written for C.

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, Exempt_Region, generate_annotations
from SUITE.gprutils import GPRswitches

# Generate external annotations, they will be generated in a dedicated
# temporary directory
annotations = generate_annotations(
    [Exempt_Region("main.c", "16:5", "18:31", "In statement")]
)

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.c"], srcdirs=[".."])),
    extra_coverage_args=["--annotate=xcov"],
    extra_instr_args=[f"--external-annotations={annotations}"],
    covlevel="stmt+mcdc",
    mains=["main"],
    trace_mode="src",
    tolerate_instrument_messages="Exemption .* intersects",
)

check_xcov_reports(
    "obj",
    {"main.c.xcov": {"+": {7, 15}, "!": {5, 17}, "-": {9}}},
)

thistest.fail_if_no_match(
    what="Unexpected instrument output",
    regexp=r"(warning: Exemption annotation at main\.c:\d+:\d+ intersects a"
    r" coverage obligation \(.*\), ignoring it\n)+",
    actual=contents_of("instrument.log"),
)

thistest.result()
