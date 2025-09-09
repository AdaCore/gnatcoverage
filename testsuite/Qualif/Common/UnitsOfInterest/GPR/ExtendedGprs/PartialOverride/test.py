"""
Check that gnatcov correctly selects units of interest of extending projects.

In particular, this checks that the units of the extended projects (which are
not overriden by the extending project) are considered to be units of interest.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

# The "orig" project contains two units: "main" and "helper". The "ext" project
# extends "orig" and overrides only the "helper" unit.
#
# Previously, gnatcov used to consider that the only unit of interest was
# "helper". It now also consider that "main" is a unit of interest.
orig_prj = gprfor(prjid="orig", mains=["main.adb"], srcdirs="..")
ext_prj = "ext.gpr"
with open(ext_prj, "w") as f:
    f.write(
        """
project Ext extends "{}" is
    for Source_Dirs use ("../src-ext");
    for Object_Dir use "obj-ext";
end Ext;
""".format(
            orig_prj
        )
    )

build_run_and_coverage(
    gprsw=GPRswitches(root_project=ext_prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
    gpr_obj_dir="obj-ext",
    gpr_exe_dir="obj-ext",
)

check_xcov_reports(
    "obj-ext",
    {
        "main.adb.xcov": {"+": {5}},
        "helper.ads.xcov": {},
        "helper.adb.xcov": {"+": {4}},
    },
    # See eng/das/cov/gnatcoverage#245
    discard_empty=False,
)

thistest.result()
