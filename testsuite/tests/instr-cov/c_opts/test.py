"""
Check that --c-opts option for "gnatcov instrument" works as expected.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


other_dir = os.path.abspath("other")
tmp = Wdir("tmp_")

c_opts = [
    # Without these arguments, "hello.h" cannot be imported, and the A macro is
    # not defined. Exercize the various forms for these arguments: -XA, -X A).
    # Also add these as two "--c-opts" arguments.
    ["-DA", "-D", "B", "-UB"],
    ["-I", other_dir],

    # Check the processing of backslashes
    ["-DBACKSLASH=\\"],
    ["-DDOUBLE_BACKSLASH=\\\\"],
    ["-DCOMMA=\\,"],
]

log_file = "instrument.log"
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=[".."], mains=["main.c"]),
        units=["main.c"],
    ),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=(
        ["--c-opts={}".format(",".join(args)) for args in c_opts]
        + ["-v"]
    ),
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

for excerpt in ["-DBACKSLASH=\\", "-DDOUBLE_BACKSLASH=\\", "-DCOMMA=,"]:
    thistest.fail_if(
        excerpt not in contents_of(log_file),
        f"{excerpt} macro definition not found in {log_file}"
    )

check_xcov_reports("xcov", {"main.c.xcov": {"+": {7, 14}}})

thistest.result()
