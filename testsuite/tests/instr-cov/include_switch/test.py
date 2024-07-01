"""
Check that gnatcov instrument correctly processes `-include` switches which can
alter the preprocessing of the file.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Test every possible way for extra safety
c_opts = [
    ["--include=../pkg1.h"],
    ["--include", "../pkg2.h"],
    ["-include", "../pkg3.h"],
]

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=[".."], mains=["main.c"]),
    ),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=(
        ["--c-opts={}".format(",".join(args)) for args in c_opts]
    ),
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "xcov",
    {
        "main.c.xcov": {"+": {4, 5, 6}},
        "pkg1.h.xcov": {"+": {6}},
        "pkg2.h.xcov": {"+": {6}},
        "pkg3.h.xcov": {"+": {6}},
    },
)

thistest.result()
