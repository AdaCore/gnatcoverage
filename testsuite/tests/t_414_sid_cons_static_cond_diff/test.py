"""
Regression test: ensure that "gnatcov coverage" can load alternative SIDs for
the same unit (here: Main) when it contains a SCO condition that has different
static evaluations in both SIDs.
"""

from e3.fs import cp, mkdir

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

# Create a project with two alternative sources for one unit (Pkg) depending on
# a GPR external.
gpr = gprfor(
    mains=["main.adb"],
    srcdirs=[".."],
    extra="""
    type Pkg_Version_Type is ("v1", "v2");
    Pkg_Version : Pkg_Version_Type := external("PKG_VERSION");
    package Naming is
        for Spec ("Pkg") use "pkg__" & Pkg_Version & ".ads";
    end Naming;
    """,
)

# Create subdirectories to group the SID and trace files for "gnatcov coverage"
# to load.
mkdir("sids")
mkdir("traces")

# In order to reproduce the crash, it was necessary to first load a SID with N
# SCO statements (here: prelude.sid, which is the same regardless of the
# selected Pkg variant), and then load the two SIDs (here: for the Main unit)
# that contain the SCO condition with different static evaluations ("if Pkg.B"
# in main.adb). Consolidation code used to use the condition SCO index from the
# SID to load (likely 2 here, SCO 1 being the statement call to Prelude) to
# wrongly subscript gnatcov's interned SCO table (SCO 2 being the second
# Put_Line statement call in prelude.adb).
sids = ["obj/prelude.sid"]
traces = []
for v in ["v1", "v2"]:
    cov_args = build_and_run(
        gprsw=GPRswitches(root_project=gpr, xvars=[("PKG_VERSION", v)]),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=[],
    )

    # Keep a side copy of the trace file and the SID for main.adb
    trace_copy = f"traces/main-{v}.srctrace"
    cp(cov_args[-1], trace_copy)
    traces.append(trace_copy)

    sid_copy = f"sids/main-{v}.sid"
    cp("obj/main.sid", sid_copy)
    sids.append(sid_copy)

sid_args = [f"--sid={f}" for f in sids]
xcov(
    ["coverage", "-cstmt", "-axcov", "--output-dir=xcov", *sid_args, *traces],
    out="coverage.log",
)
check_xcov_reports(
    "xcov",
    {
        "prelude.adb.xcov": {"+": {5, 6, 7, 8}},
        "main.adb.xcov": {"+": {8, 9, 12}},
    },
)

thistest.result()
