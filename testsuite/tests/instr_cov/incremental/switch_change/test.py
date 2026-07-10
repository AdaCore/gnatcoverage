"""
Check that when running gnatcov instrument with varying switches, gnatcov
reinstruments the files.
"""

import os

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir()


all_sources = {"main.adb", "pkg.ads", "bar.c", "baz.cpp"}


def check_instrumented_sources(label: str, instr_sources: set[str]) -> None:
    thistest.fail_if_not_equal(
        f"[{label}] instrumented sources",
        "\n".join(sorted(instr_sources)),
        "\n".join(
            sorted(set(os.listdir("obj/gen-gnatcov-instr")) & all_sources)
        ),
    )


def instrument_and_check(
    label: str,
    args: list[str],
    instr_sources: set[str],
) -> None:
    thistest.log(f"== {label} ==")
    tmp.to_subdir(f"tmp_{label}")

    # Create a project
    root_prj = gprfor(
        srcdirs=[".."], langs=["Ada", "C", "C++"], mains=["main.adb"]
    )

    # Run the instrumenter a first time for statement coverage
    xcov_instrument(gprsw=GPRswitches(root_project=root_prj), covlevel="stmt")

    # At this point, all sources should be instrumented (this is just a sanity
    # check).
    check_instrumented_sources("first", all_sources)

    # Run the instrumenter a second time, to check incrementality. Unless the
    # purpose of this check is to verify the influence of a change of coverage
    # level, still instrument for statement coverage.
    if "--level" not in args:
        args.append("--level=stmt")
    xcov_instrument(
        gprsw=GPRswitches(root_project=root_prj),
        covlevel=None,
        extra_args=args,
        quiet=False,
        out="instrument.out",
    )

    # The non-quiet output from "gnatcov instrument" shows which unit was
    # re-instrumented: units not mentionned in the output were not
    # re-instrumented.
    thistest.fail_if_diff(
        baseline_file=f"../instrument-{label}.expected",
        actual_file="instrument.out",
    )

    # Now, only expected sources should be instrumented. This is to check that
    # instrumented sources for units that were instrumented the first time, but
    # no longer units of interest the second time, were correctly deleted.
    check_instrumented_sources("second", instr_sources)


# Change the list of units of interest: still-of-interest units do not need to
# be reinstrumented, and no-longer-of-interest units should be deleted by the
# "clean_objdirs" pass.
instrument_and_check(
    "change-uoi",
    ["--units=main"],
    {"main.adb"},
)

# Change the coverage level. It should reinstrument every file.
instrument_and_check("change-covlevel", ["--level", "stmt+mcdc"], all_sources)

# Change a dump option. It should reinstrument only main sources: the only
# argument that changes under the hood is that --dump-filename-simple is passed
# to gnatcov instrument-source" for the main.
instrument_and_check("change-dump", ["--dump-filename-simple"], all_sources)

thistest.result()
