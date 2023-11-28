"""
Check that "gnatcov instrument" correctly reports progress about the
instrumented units.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Avoid "creating output path" info messages
os.mkdir("obj")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            langs=["Ada", "C", "C++"],
            mains=["main.adb"],
            srcdirs=[".."],
        )
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    quiet=False,
    trace_mode="src",
)

# Sanity check: the insrument-build-coverage process completed with the
# expected results.
check_xcov_reports(
    "*xcov",
    {
        "main.adb.xcov": {"+": {7, 8}},
        "c_unit.c.xcov": {"+": {8}},
        "cpp_unit.cpp.xcov": {"+": {10}},
    },
    cwd="xcov",
)

# Units are not instrumented in a particular order: we only want to check that
# all of them are listed with the expected formatting.
output = lines_of("instrument.log")
thistest.fail_if_not_equal(
    "First line of 'gnatcov instrument' output",
    "".join(output[:1]),
    "Coverage instrumentation",
)
thistest.fail_if_not_equal(
    "'gnatcov instrument' output",
    "\n".join([
        "   [Ada]           main",
        "   [C++]           cpp_unit.cpp",
        "   [C]             c_unit.c",
    ]),
    "\n".join(sorted(output[1:])),
)

thistest.result()
