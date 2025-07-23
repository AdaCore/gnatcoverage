"""
Check that gnatcov can deal with C/C++ code in another standard than the
default.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor
from SUITE.gprutils import GPRswitches


def gen_prj():
    return GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.cpp"]))


tmp = Wdir("tmp_valid-instrumentation")

# Check that passing std=c++20 to both the instrumenter, and the builder,
# compiles and produces the right coverage report.

build_run_and_coverage(
    gprsw=gen_prj(),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--c++-opts=-std=c++20"],
    extra_gprbuild_cargs=["-std=c++20"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "xcov", {"main.cpp.xcov": {"+": {6, 7, 8, 9, 11, 13, 16}, "-": {12, 14}}}
)

tmp.to_homedir()

# Now check that when we do not pass std=c++20 to the instrumenter, we
# produce invalid code, that will fail to compile.
#
# TODO: gnatcov instrument should emit warnings. We get diagnostics when
# parsing with clang, but we choose not to emit them, as we are bound to get
# diagnostics even for valid code. One thing we could try would be to filter
# out the diagnostics that we will surely get (references to gcc builtins for
# instance), and print the other ones.

tmp = Wdir("tmp_invalid-instrumentation")

gprsw = gen_prj()
xcov_instrument(gprsw=gprsw, covlevel="stmt")
p = gprbuild(
    gprsw.root_project,
    extracargs=["-std=c++20"],
    trace_mode="src",
    register_failure=False,
)

thistest.fail_if(p.status == 0, "Expecting compilation failure")

thistest.result()
