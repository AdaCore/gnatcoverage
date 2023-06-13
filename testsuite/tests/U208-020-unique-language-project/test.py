"""
This test checks that the instrumentation of an Ada project produces
only Ada files. Likewise for C.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.control import LANGINFO
from SUITE.cutils import ext, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(
            prjid="ada_prj", srcdirs=[".."],
            mains=["main.adb"], langs=["Ada"]
        )),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"])

for f in os.listdir("obj/ada_prj-gnatcov-instr"):
    if ext(f) not in LANGINFO["Ada"].src_ext:
        thistest.failed(
            f"{f} is not an Ada source. Instrumentation "
            "of an Ada project should only produce Ada files.")
        break

check_xcov_reports('obj/main.adb.xcov', {'obj/main.adb.xcov': {'+': {3}}})

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(prjid="c_prj", srcdirs=[".."], mains=["main.c"], langs=["C"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"])

for f in os.listdir("obj/c_prj-gnatcov-instr"):
    if ext(f) not in LANGINFO["C"].src_ext:
        thistest.failed(
            f"{f} is not a C source. Instrumentation "
            "of a C project should only produce C files.")
        break

check_xcov_reports('obj/main.c.xcov', {'obj/main.c.xcov': {'+': {4}}})
thistest.result()
