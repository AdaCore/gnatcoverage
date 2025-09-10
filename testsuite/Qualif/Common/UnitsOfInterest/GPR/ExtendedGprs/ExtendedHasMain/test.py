"""
Check that source files in extended projects are considered when looking for
mains.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor


wd = Wdir("tmp_")

# Prepare the two project files
p = gprfor(prjid="p", mains=["main1.adb"], srcdirs=["../src"], objdir="obj-p")
ext_p = "ext_p.gpr"
with open(ext_p, "w") as f:
    f.write(
        """
        project Ext_P extends "{}" is
            for Source_Dirs use ("../ext-src");
            for Object_Dir use "ext-obj";
            for Exec_Dir use ".";
            for Main use ("main1.adb", "main2.adb");
        end Ext_P;
    """.format(
            p
        )
    )

build_run_and_coverage(
    gprsw=GPRswitches(root_project=ext_p),
    covlevel="stmt",
    mains=["main1", "main2"],
    gpr_obj_dir="ext-obj",
    extra_coverage_args=["-axcov"],
)

# In blob.ads, we expect either '+' or '.' on everything, depending
# on the toolchain. Let check_xcov_reports refine that.

check_xcov_reports(
    "ext-obj",
    {
        "blob.ads.xcov": {},
        "main1.adb.xcov": {"+": {5}},
        "main2.adb.xcov": {"+": {5, 6}},
    },
    # See eng/das/cov/gnatcoverage#245
    discard_empty=False,
)

thistest.result()
