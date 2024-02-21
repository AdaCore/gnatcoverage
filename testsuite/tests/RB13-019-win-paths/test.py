"""
Check that gnatcov does not complain about "same base name for files" and
computes the expected code coverage when C files are compiled with a Makefile.

This used not to work because in this build scenario on Windows, debug info
used contains unexpected double backslashes as directory separators.
"""

import os.path
import shutil

from e3.fs import cp
from e3.os.process import PIPE, Run, STDOUT

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.control import env
from SUITE.cutils import FatalError, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    exepath_to, gprfor, thistest, tracename_for, xcov, xrun
)


tmp = Wdir("tmp_")


# Copy the C project in this temporary directory to avoid clutter in the test
# sources.
for f in ["Makefile", "libsrc"]:
    cp(os.path.join("..", f), f, recursive=True)

# Build libdriver.a
p = Run(
    [
        "make",
        f"CC={env.target.triplet}-gcc",
        f"AR={env.target.triplet}-ar",
        "CFLAGS={}".format(" ".join([
            "-fpreserve-control-flow",
            "-fdump-scos",
            "-g",
            "-save-temps",

            # TODO??? This is necessary in order to have the C sources build
            # with an arm-elf toolchain and a stm32f4 RTS, it is not clear if
            # we could make this more generic.
            "-mlittle-endian",
            "-mfloat-abi=hard",
            "-mcpu=cortex-m4",
            "-mfpu=fpv4-sp-d16",
        ])),
    ],
    output=PIPE,
    error=STDOUT,
)
thistest.log("make output:\n" + p.out)
thistest.stop_if(p.status != 0, FatalError('call to "make" failed'))

# Build the test program, run it and produce a coverage report
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            prjid="main",
            srcdirs=[".."],
            mains=["main.adb"],
            langs=["Ada"],
        ),
    ),
    covlevel="stmt+mcdc",
    mains=["main"],
    scos=["obj/main"],
    extra_gprbuild_args=["-vh"],
    extra_gprbuild_largs=[f"-L{os.getcwd()}", "-ldriver"],
    extra_coverage_args=[
        "-axcov",
        "--output-dir=coverage",
        "--scos=wibble.c.gli",
        "--scos=driver.c.gli",
    ],
)

check_xcov_reports("coverage", {
    "wibble.c.xcov": {"+": {8, 12}, "!": {6}},
    "main.adb.xcov": {"+": {7}},
    "driver.c.xcov": {"+": {13, 24, 25, 27}, "!": {11}, "-": {17}},
})

thistest.result()
