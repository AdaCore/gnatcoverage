from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["../src"], mains=["dom_debug.adb"])
    ),
    covlevel="stmt",
    mains=["dom_debug"],
    extra_coverage_args=["--annotate=xcov"],
    scos=["obj/dom_debug"],
    extra_gprbuild_args=["-O0"],
)

# We expect the first variable declaration to be covered (through back
# propagation through disabled dominant pragma Debug).
thistest.fail_if(
    not match(r"[0-9]+ \+:.*X0 : Integer;", "dom_debug.adb.xcov"),
    "expect X0 to be covered",
)

# The pragma itself is disabled and must be reported as NO CODE
thistest.fail_if(
    not match(r"[0-9]+ \.:.*pragma Debug", "dom_debug.adb.xcov"),
    "expect pragma Debug to be marked no-code",
)
thistest.result()
