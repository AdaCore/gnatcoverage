"""
Check that the source trace produced by gnatcov matches the executable name,
and not the unit name. The two can be different when the executable name is
specified in the project file, or when the unit name and the filename are
mismatching. Test both cases.
"""

from SCOV.minicheck import build_and_run
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

import os


def check_srctrace_name(gprsw, main):
    build_and_run(
        gprsw=gprsw,
        covlevel="stmt",
        mains=[main],
        extra_instr_args=["--dump-filename-simple"],
        extra_coverage_args=[],
    )
    # Check the name of the trace
    trace_ext = (
        ".srctrace" if thistest.options.trace_mode == "src" else ".trace"
    )
    trace_name = f"{main}{env.target.os.exeext}{trace_ext}"
    thistest.fail_if(
        not os.path.exists(trace_name),
        f"Could not find {trace_name}",
    )


# Check when the executable name is left unspecified
tmp = Wdir("tmp_no_exec_attr")
gprsw = GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."]))
check_srctrace_name(gprsw, "main")

# Check when the executable name is specified in the project
tmp.to_subdir("tmp_exec_attr")
gprsw = GPRswitches(
    root_project=gprfor(
        mains=["main.adb"],
        srcdirs=[".."],
        extra="""package Builder is
                    for Executable ("main.adb") use "bar";
                 end Builder;""",
    )
)
check_srctrace_name(gprsw, "bar")

thistest.result()
