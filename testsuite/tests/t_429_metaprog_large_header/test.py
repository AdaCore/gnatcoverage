"""
Check that coverage obligations relocated to their inclusion point get valid
source locations even when the included file is larger than 32KB.

Low level SCO column numbers are bounded by 2**15, so relocated SCO columns
must not be encoded as byte offsets in the included file (this used to crash
the instrumenter with a Constraint_Error on such headers).
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

# Generate a header bigger than 2**15 bytes whose code lies at the end, so
# that the byte offsets of the relocated coverage obligations do not fit in
# a low level SCO column number.

with open("big.h", "w") as f:
    for _ in range(600):
        f.write("/* " + "x" * 58 + " */\n")
    f.write("int a = 0;\n")
    f.write("int b = 0;\n")
    f.write("return a + b;\n")

with open("test.c", "w") as f:
    f.write('int\nmain ()\n{\n#include "big.h"\n}\n')

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=["."], mains=["test.c"])),
    covlevel="stmt",
    mains=["test"],
    extra_coverage_args=["--annotate=xcov"],
)

check_xcov_reports("obj", {"test.c.xcov": {"+": {4}}})

thistest.result()
