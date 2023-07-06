"""
This test checks that gnatcov correctly instruments projects with a complex
structure, and with unit parts implemented in different projects of the project
tree, some of them being instrumented while some of them not (through project
extension).
"""

import os

from e3.fs import cp

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import no_ext, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprinstall


Wdir("tmp_")

# Start by installing the library. Every main subproject depends on it.
install_dir = os.path.abspath("install")
cp(os.path.join("..", "src-lib"), ".", recursive=True)
lib_prj = os.path.join("src-lib", "lib.gpr")
xcov_instrument(
    gprsw=GPRswitches(root_project=lib_prj),
    covlevel="stmt",
)
gprbuild(lib_prj)
gprinstall(
    lib_prj,
    [
        f"--prefix={install_dir}",
        "--src-subdirs=gnatcov-instr",
        "--implicit-with=gnatcov_rts",
    ],
)

# Add the newly installed library to gprbuild's project lookup path. Use the
# ADA_PROJECT_PATH environment variable to be compatible with the 5.04
# toolchain.
gpr_install_dir = os.path.join(install_dir, "share", "gpr")
old_path = os.environ.get("ADA_PROJECT_PATH", "")
if old_path:
    new_path = "{}{}{}".format(gpr_install_dir, os.path.pathsep, old_path)
else:
    new_path = gpr_install_dir
os.environ["ADA_PROJECT_PATH"] = new_path


def build_run_cov_and_check(main_prj, main_prj_obj_dir, expected_xcov):
    cp(os.path.join("..", main_prj), ".", recursive=True)
    cp(os.path.join("..", f"src-{no_ext(main_prj)}"), ".", recursive=True)
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=os.path.join(main_prj)),
        covlevel="stmt",
        mains=["main"],
        gpr_obj_dir=main_prj_obj_dir,
        gpr_exe_dir=main_prj_obj_dir,
        extra_coverage_args=[
            "--annotate=xcov",
            "--externally-built-projects",
            "--units=pkg",
        ],
    )
    check_xcov_reports("*.xcov", expected_xcov, main_prj_obj_dir)


# Try the simple case: the main project extends the library, but does not
# redefine anything.
thistest.log("== Simple case ==")
expected_xcov = {"pkg.adb.xcov": {"+": {5}}, "pkg-bar.adb.xcov": {"+": {3}}}
build_run_cov_and_check(
    "main_simple.gpr",
    main_prj_obj_dir="obj-main_simple",
    expected_xcov=expected_xcov,
)

# Now the main project defines an alternate body. TODO! as we reinstrument the
# whole unit if a part of it was redefined (including the parts that were not
# redefined), we end up instrumenting the instrumented version for sources
# belonging to the library. We should fix this (and adjust coverage
# expectations) if possible.
thistest.log("== Redefining Pkg body ==")
expected_xcov = {"pkg.adb.xcov": {"+": {7}}, "pkg-bar.adb.xcov": {"+": {4}}}
# Should be: {"pkg.adb.xcov": {"+": {7}}, "pkg-bar.adb.xcov": {"+": {5}}}
build_run_cov_and_check(
    "main_body.gpr",
    main_prj_obj_dir="obj-main_body",
    expected_xcov=expected_xcov,
)


# Now the main project defines an alternate separate.
thistest.log("== Redefining Pkg.Bar separate ==")
expected_xcov = {"pkg.adb.xcov": {"+": {6}}, "pkg-bar.adb.xcov": {"+": {5}}}
# Should be: {"pkg.adb.xcov": {"+": {5}}, "pkg-bar.adb.xcov": {"+": {5}}}
build_run_cov_and_check(
    "main_sep.gpr",
    main_prj_obj_dir="obj-main_sep",
    expected_xcov=expected_xcov,
)

# Now the main project defines an alternate spec.
thistest.log("== Redefining Pkg spec ==")
expected_xcov = {
    "pkg.adb.xcov": {"+": {6}},
    "pkg-bar.adb.xcov": {"+": {4}},
    "pkg.ads.xcov": {"+": {4}},
}
# Should be: {
#    "pkg.adb.xcov": {"+": {5}},
#    "pkg-bar.adb.xcov": {"+": {3}},
#    "pkg.ads.xcov": {"+": {4}},
# }
build_run_cov_and_check(
    "main_spec.gpr",
    main_prj_obj_dir="obj-main_spec",
    expected_xcov=expected_xcov,
)

thistest.result()
