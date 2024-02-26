"""
Regression testcase: check that the instrumentation of a C source including a
header from a project dependency works.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


def create_lib_gpr(name, deps=[]):
    """
    Create a gpr file for the library.

    :param name: Name of the library.
    """

    return gprfor(
        mains=[],
        prjid=name,
        srcdirs=f"../src-{name}",
        langs=["C"],
        deps=deps,
        extra=f"""
                      for Library_Name use "{name}";
                      for Library_Dir use "{name}";
                  """,
    )


tmp = Wdir("tmp_")

# Create the GPR file for the root project
src_gpr = gprfor(
    mains=["main.c"],
    prjid="main",
    srcdirs="../src",
    objdir="obj",
    langs=["C"],
    deps=["lib1", "lib2"],
)

# Create the GPR files for the libraries
lib1_gpr = create_lib_gpr("lib1", ["lib2"])
lib2_gpr = create_lib_gpr("lib2")

gprsw = GPRswitches(root_project=src_gpr)

build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

check_xcov_reports(
    "obj",
    {
        "main.c.xcov": {"+": {6, 7, 8}},
        "pkg1.c.xcov": {"+": {7, 8}},
        "pkg2.c.xcov": {"+": {6}},
    },
)

thistest.result()
