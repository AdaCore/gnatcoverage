"""
Test that gnatcov can deal with aggregate projects with a single aggregated
project.
"""

import os.path

from e3.fs import mkdir

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

# Generate a project that cannot be loaded directly because the GPR file and
# its dependencies are not in the current directory nor in GPR_PROJECT_PATH.
projects_dir = os.path.abspath("projects")
lib_obj_dir = os.path.join(projects_dir, "obj-lib")
main_obj_dir = os.path.join(projects_dir, "obj-main")

mkdir(projects_dir)
lib_gpr = gprfor(
    prjid="lib",
    mains=[],
    srcdirs=[os.path.abspath("../src-lib")],
    objdir=lib_obj_dir,
    cwd=projects_dir,
)
main_gpr = gprfor(
    prjid="main",
    mains=["main.adb"],
    srcdirs=[os.path.abspath("..")],
    objdir=main_obj_dir,
    exedir=os.getcwd(),
    deps=["lib"],
    cwd=projects_dir,
)

# Generate an aggregate project that gives access to that project tree
agg_gpr = os.path.abspath("agg.gpr")
with open(agg_gpr, "w") as f:
    f.write(
        """
        aggregate project Agg is
            for Project_Path use ("projects");
            for Project_Files use ("projects/main.gpr");
        end Agg;
        """
    )

build_run_and_coverage(
    gprsw=GPRswitches(root_project=agg_gpr),
    covlevel="stmt",
    mains=["main"],
    gpr_obj_dir=main_obj_dir,
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)
check_xcov_reports(
    "xcov",
    {"main.adb.xcov": {"+": {7, 8}}, "lib.ads.xcov": {}},
    discard_empty=False,
)

thistest.result()
