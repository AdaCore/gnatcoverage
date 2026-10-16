"""
Check that the source trace produced by gnatcov matches the executable name,
and not the unit name. The two can be different when the executable name is
specified in the project file.
"""

import os

from SCOV.minicheck import build_and_run
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor


tmp = Wdir("tmp_")
build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(
            mains=["main.adb"],
            srcdirs=[".."],
            extra="""
                package Builder is
                   for Executable ("main.adb") use "bar";
                end Builder;
            """,
        )
    ),
    covlevel="stmt",
    mains=["bar"],
    extra_instr_args=["--dump-filename-simple"],
    extra_coverage_args=[],
)

# Check the name of the trace
trace_name = f"bar{env.target.os.exeext}.srctrace"
thistest.fail_if(
    not os.path.exists(trace_name), f"Could not find {trace_name}"
)

thistest.result()
