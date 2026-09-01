"""
Regression testcase for the computation of coverage buffer symbol slugs for C
sources. gnatcov computes them from source file full names: when instrumenting
a project that depends on an installed (externally built) instrumented
library, gnatcov used to recompute the library buffer symbols from the
installed source full names, which do not match the symbols actually defined
in the library (computed from the original source full names at library
instrumentation time), leading to an undefined symbol error at link time.

The library buffers are now reached through the buffers group array symbol of
the library project, aggregated in the buffers group array list of the
depending project. Check the three features that go through that list: the
automatic buffer dump, the manual buffer dump/reset (buffer control
annotations) and the live observability API.
"""

import glob
import os.path

from e3.fs import cp

from SCOV.instr import xcov_instrument
from SCOV.minicheck import (
    build_and_run,
    build_run_and_coverage,
    check_xcov_reports,
)
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    gprbuild,
    gprfor,
    gprinstall,
    xcov,
)

tmp = Wdir("tmp_build")

# Instrument, build and install the library project
cp("../mylib", ".", recursive=True)
mylib_gpr = os.path.join("mylib", "mylib.gpr")

install_dir = os.path.abspath("install")
gpr_install_dir = os.path.join(install_dir, "share", "gpr")
env.add_search_path("GPR_PROJECT_PATH", gpr_install_dir)

xcov_instrument(
    gprsw=GPRswitches(root_project=mylib_gpr),
    covlevel="stmt",
    gpr_obj_dir=os.path.join("mylib", "obj"),
)
gprbuild(mylib_gpr, trace_mode="src")
gprinstall(
    mylib_gpr,
    [
        f"--prefix={install_dir}",
        "--src-subdirs=gnatcov-instr",
        "--implicit-with=gnatcov_rts",
    ],
)

installed_mylib_gpr = os.path.join(gpr_install_dir, "mylib.gpr")

# ----------------------------------------------------------------------------
# Automatic dump scenario: instrument, build and run the main project, which
# depends on the installed library, then compute its coverage. The buffers
# group array list that the automatic dump helper references must aggregate
# the buffers group array defined in the installed library.

tmp.to_subdir("tmp_auto")
thistest.log("======== Automatic dump =========")
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            prjid="main",
            mains=["main.c"],
            deps=[installed_mylib_gpr],
            srcdirs=["../main"],
        ),
        externally_built_projects=True,
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "obj", {"main.c.xcov": {"+": {6, 7}}, "foo.c.xcov": {"+": {6}}}
)

# ----------------------------------------------------------------------------
# Buffer control annotations scenario: dump ("t1"), then reset, then dump
# again ("t2") through in-source annotations in the main source. The "t1"
# trace must contain the coverage data of the library (dump helpers go
# through the buffers group array list), and the reset must clear the
# library buffers, so the "t2" trace must report the library function as not
# covered.

tmp.to_subdir("tmp_manual")
thistest.log("======== Buffer control annotations =========")
cov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(
            prjid="main_manual",
            mains=["main_manual.c"],
            deps=[installed_mylib_gpr],
            srcdirs=["../main_manual"],
        ),
        externally_built_projects=True,
    ),
    covlevel="stmt",
    mains=["main_manual"],
    extra_coverage_args=["--annotate=xcov"],
    extra_instr_args=["--dump-filename-simple"],
    dump_trigger="manual",
    manual_prj_name="main_manual",
    trace_mode="src",
)

traces = sorted(glob.glob("t[0-9].srctrace"))
thistest.fail_if(
    traces != ["t1.srctrace", "t2.srctrace"],
    comment=f"unexpected traces: {traces}",
)

expected_cov = {
    "t1": {
        "main_manual.c.xcov": {"+": {6}, "-": {9, 11}},
        "foo.c.xcov": {"+": {6}},
    },
    "t2": {
        "main_manual.c.xcov": {"+": {9}, "-": {6, 11}},
        "foo.c.xcov": {"-": {6}},
    },
}
for trace in traces:
    prefix = trace.split(".")[0]
    output_dir = f"output_{prefix}"
    xcov(
        cov_args + [f"--output-dir={output_dir}", trace],
        out=f"coverage_{prefix}.log",
    )
    check_xcov_reports(output_dir, expected_cov[prefix])

# ----------------------------------------------------------------------------
# Observability scenario: an Ada main queries the number of bits set in the
# coverage buffers before and after calling the library function. The
# generated Sum_Buffer_Bits function goes through the buffers group array
# list, so it must observe the buffers of the installed library (the only
# unit of interest here).

tmp.to_subdir("tmp_obs")
thistest.log("======== Observability =========")
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            prjid="main_obs",
            mains=["main_obs.adb"],
            deps=[installed_mylib_gpr],
            srcdirs=["../main_obs"],
        ),
        projects=["mylib"],
        externally_built_projects=True,
    ),
    covlevel="stmt",
    mains=["main_obs"],
    extra_coverage_args=["--annotate=xcov"],
    trace_mode="src",
)

check_xcov_reports("obj", {"foo.c.xcov": {"+": {6}}})

output = contents_of("main_obs_output.txt")
thistest.fail_if_not_equal(
    what="wrong buffer bit counts in observability main output",
    expected="Before: 0\nAfter: 1",
    actual=output.strip(),
)

thistest.result()
