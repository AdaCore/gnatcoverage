"""
Check that we do not have CRCRLF line endings in instrumented files. This used
to be the case on windows, as we were opening the instrumented file as a text
file, and not as a binary file when writing it.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir('tmp_')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov', '--output-dir=xcov'],
    trace_mode='src')

# Check line endings
with open(os.path.join("obj", "gen-gnatcov-instr", "main.adb"), "rb") as f:
    content = f.read()
    thistest.fail_if(
        b'\r\r\n' in content,
        comment="wrong line ending in instrumented source",
    )

check_xcov_reports('xcov/*.xcov', {
    'xcov/main.adb.xcov': {'+': {5}},
})

thistest.result()
