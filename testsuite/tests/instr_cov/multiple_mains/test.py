"""
Check that the instrumentation of multiple mains, with one of the main being
in the closure of the other, works.

With gnatcov instrument, we used to generate a unique C symbol for the
Dump_Buffer procedure, that resulted in symbol name clashing when linking.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=["../src"], mains=["test_a.adb", "test_b.adb"]
        )
    ),
    covlevel="stmt",
    mains=[],
    extra_coverage_args=[],
    trace_mode="src",
)

thistest.result()
