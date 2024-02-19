"""
Check that the instrumentation of a C project does not instrument the header
files: we only instrument the code in header files once it has been included in
a ".c" file *when* we instrument that ".c" file.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

# Build and produce a coverage report for the test project.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src',
)
check_xcov_reports(
    'xcov',
    {
        'main.c.xcov': {'+': {6, 7}},
        'fact.h.xcov': {'+': {4, 5, 7}},
    },
)

# Check that the header file is not instrumented
instr_header = os.path.join("obj", "gen-gnatcov-instr", "fact.h")
thistest.fail_if(
    os.path.isfile(instr_header),
    f"Found spurious {instr_header} file",
)

thistest.result()
