"""
Check that when running gnatcov instrument with varying switches, gnatcov
reinstruments the files.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir()


def instrument_and_check(args: list[str], label: str) -> None:
    tmp.to_subdir(f"tmp_{label}")

    # Create a project
    root_prj = gprfor(
        srcdirs=[".."], langs=["Ada", "C", "C++"], mains=["main.adb"]
    )

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
    thistest.fail_if_diff(
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
