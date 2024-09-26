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
from SUITE.control import env
from SUITE.cutils import Wdir
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
env.add_search_path(
    "ADA_PROJECT_PATH", os.path.join(install_dir, "share", "gpr")
)


def check(label, name, expected_xcov):
    """
    Copy the "name" project from the test directory to the current directory
    and run it through the instrument/build/run/coverage steps. Check that the
    coverage report produced matches "expected_cov".
    """
    thistest.log(f"== {label} ({name}) ==")

    main_prj = f"{name}.gpr"
    obj_dir = f"obj-{name}"

    cp(os.path.join("..", main_prj), ".", recursive=True)
    cp(os.path.join("..", f"src-{name}"), ".", recursive=True)

    build_run_and_coverage(
        gprsw=GPRswitches(root_project=os.path.join(main_prj)),
        covlevel="stmt",
        mains=["main"],
        gpr_obj_dir=obj_dir,
        gpr_exe_dir=obj_dir,
        extra_coverage_args=[
            "--annotate=xcov",
            "--externally-built-projects",
            "--units=pkg",
        ],
    )
    check_xcov_reports(obj_dir, expected_xcov)


# Try the simple case: the main project extends the library, but does not
# redefine anything.
check(
    "Simple case",
    "main_simple",
    expected_xcov={
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {5}},
        "pkg-bar.adb.xcov": {"+": {3}},
    },
)

# Now the main project defines an alternate body. TODO! as we reinstrument the
# whole unit if a part of it was redefined (including the parts that were not
# redefined), we end up instrumenting the instrumented version for sources
# belonging to the library. We should fix this (and adjust coverage
# expectations) if possible.
check(
    "Redefining Pkg body",
    "main_body",
    expected_xcov={
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {7}},
        "pkg-bar.adb.xcov": {"+": {3}},
    },
)


# Now the main project defines an alternate separate.
check(
    "Redefining Pkg.Bar separate",
    "main_sep",
    expected_xcov={
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {5}},
        "pkg-bar.adb.xcov": {"+": {5}},
    },
)

# Now the main project defines an alternate spec.
check(
    "Redefining Pkg spec",
    "main_spec",
    expected_xcov={
        "pkg.adb.xcov": {"+": {5}},
        "pkg-bar.adb.xcov": {"+": {3}},
        "pkg.ads.xcov": {"+": {4}},
    },
)

thistest.result()
