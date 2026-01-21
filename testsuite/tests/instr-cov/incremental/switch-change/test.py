"""
Check that when running gnatcov instrument with varying switches, gnatcov
reinstruments the files.
"""

import os.path
import shutil

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Create a project
root_prj = gprfor(
    srcdirs=[".."], langs=["Ada", "C", "C++"], mains=["main.adb"]
)


def instrument_and_check(args, label):
    # Remove existing instrumentation artifacts
    if os.path.exists("obj"):
        shutil.rmtree("obj")

    # Start with a statement instrumentation, and then check incrementality
    xcov_instrument(gprsw=GPRswitches(root_project=root_prj), covlevel="stmt")

    if "--level" not in args:
        args.append("--level=stmt")
    xcov_instrument(
        gprsw=GPRswitches(root_project=root_prj),
        covlevel=None,
        extra_args=args,
        out="instrument.out",
    )
    thistest.stop_if_diff(
        baseline_file=f"../instrument-{label}.expected",
        actual_file="instrument.out",
    )


env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")

# Change the list of units of interest. It should reinstrument every file.
instrument_and_check(["--units=main"], "change-uoi")

# Change the coverage level. It should reinstrument every file.
instrument_and_check(["--level", "stmt+mcdc"], "change-covlevel")

# Change a dump option. It should reinstrument every file.
instrument_and_check(["--dump-filename-simple"], "change-dump")

thistest.result()
