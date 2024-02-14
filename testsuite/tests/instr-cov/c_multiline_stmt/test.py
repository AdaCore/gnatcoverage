"""
Regression testcase: checks that gnatcov can correctly instrument a statement
body of a control flow statement, when it spans on several lines and has at
least one token separating the statement expression and the terminating
semicolon, e.g.

  if <cond>
    <stmt_expr>
    // dumb-token
    ;

should be instrumented as

  if (<cond>)
  {
     <stmt_expr>
     // dumb-token
     ;
  }

gnatcov used to insert the closing brace before the terminating semicolon.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_with_main")

# Check when the main is a unit of interest
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "xcov",
    {
        "main.c.xcov": {"+": {6, 12, 17, 26, 31, 32}, "-": {27, 28}},
        "pkg.c.xcov": {"+": {4}},
    },
)

tmp.to_homedir()
tmp = Wdir("tmp_without_main")

# Also check when the main needs to be instrumented to dump the buffers with
# main-end but is not a unit of interest. We used to surround the return
# statement with braces, as we needed to declare a variable holding the return
# expression beforehand. This variable is now declared at the beginning of the
# main.
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=[".."], mains=["main.c"]),
        units=["pkg.c"],
    ),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="main-end",
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports("xcov", {"pkg.c.xcov": {"+": {4}}})

thistest.result()
