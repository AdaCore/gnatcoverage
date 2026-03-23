"""
Check that gnatcov's instrumenter ignores manual dump annotations in externally
built projects even when --externally-built-projects is passed.
"""

from e3.fs import find

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprfor


tmp = Wdir("tmp_")

# Build uninstrumented "mylib.gpr"
mylib_gpr = gprfor(
    prjid="mylib",
    srcdirs="../mylib",
    objdir="obj-mylib",
    mains=[],
    extra="""
        for Library_Name use "mylib";
        for Library_Dir use "lib-mylib";

        type Boolean is ("False", "True");
        Ext_Built : Boolean := external ("EXT_BUILT", "True");
        for Externally_Built use Ext_Built;

        package Coverage is
          for Units use ();
        end Coverage;
    """,
)
gprbuild(mylib_gpr, gargs=["-XEXT_BUILT=False"])

# Build and run "main.adb" from "main.gpr", which depends on "mylib.gpr". If
# manual dump expansion processed mylib's sources, we would get a warning
# saying that the instrumenter has found a manual dump in a subproject.
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            prjid="main",
            mains=["main.adb"],
            deps=[mylib_gpr],
            srcdirs=["../main"],
        ),
        externally_built_projects=True,
    ),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="manual",
    manual_prj_name="main",
    extra_coverage_args=["--annotate=xcov"],
)
check_xcov_reports("obj", {"main.adb.xcov": {"+": {5}}})

# As a double check in addition to the warning detection mentionned above, make
# sure that no instrumented sources were created for mylib.
thistest.fail_if_not_equal(
    "instrumented sources for mylib",
    "",
    "\n".join(sorted(find("obj-mylib", "*.ads") + find("obj-mylib", "*.adb"))),
)

thistest.result()
