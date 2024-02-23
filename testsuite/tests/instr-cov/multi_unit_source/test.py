"""
Check that the instrumentation of a project containing a multi-unit source does
not crash.
"""

import os
import os.path

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

extra = """
   package Naming is
      for Spec ("U1") use "code.ada" at 1;
      for Spec ("U2") use "code.ada" at 2;
      for Body ("U2") use "code.ada" at 3;
      for Body ("U3") use "code.ada" at 4;
   end Naming;
"""

mkdir("obj")
prj = gprfor(srcdirs=[os.path.join("..", "src")], mains=["main.adb"], extra=extra)

# Check that the instrumentation crashes and does not leave stale
# instrumentation artefacts when trying to instrument the multi-unit source.
xcov_instrument(gprsw=GPRswitches(prj), covlevel="stmt", register_failure=False)
thistest.fail_if_no_match(
    "gnatcov instrument output",
    (
        ".*gnatcov.*: instrumentation failed for .*code.ada\n"
        ".*gnatcov.*: source files containing multiple compilation units are not"
        " supported"
    ),
    contents_of("instrument.log"),
)
thistest.fail_if(
    os.path.exists(os.path.join("obj", "gen-gnatcov-instr", "main.adb")),
    "unexpected instrumented files",
)

# Now check that everything runs fine when we exclude the culprit multi-unit
# source from the coverage analysis.
build_run_and_coverage(
    gprsw=GPRswitches(prj),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--units=pck"],
    extra_coverage_args=["--units=pck", "-axcov"],
)

check_xcov_reports(
    "obj", {"pck.adb.xcov": {"+": {9, 10, 11}}, "pck.ads.xcov": {}}
)

thistest.result()
