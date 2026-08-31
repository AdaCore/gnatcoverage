"""
Check the contents of XML reports to verify that coverage origins
are correctly reported.
"""

import os

from SCOV.minicheck import build_and_run, xcov, run_cov_program
from SUITE.cutils import Wdir, FilePathRefiner
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, exepath_to

wd = Wdir("tmp_")

gpr = gprfor(["main.adb"], srcdirs="..")

os.mkdir("traces")


def delete_traces() -> None:
    for trace in os.scandir("traces"):
        os.remove(trace)


def check_xml(expected: str) -> None:
    thistest.fail_if_diff(
        os.path.join("..", expected),
        os.path.join("obj", "main.adb.xml"),
        output_refiners=[FilePathRefiner()],
    )


def instr_and_run_zero_one() -> list[str]:
    # Instrument main.adb and run the executable with arguments 0 and 1.
    # Rename the resulting trace files "zero.srctrace" and "one.srctrace" and
    # place them in "traces/".
    xcov_args = build_and_run(
        gprsw=GPRswitches(root_project=gpr),
        covlevel="stmt+mcdc+atcc+fun_call+gexpr",
        mains=["main"],
        extra_coverage_args=[],
        extra_instr_args=["--dump-filename-simple", "--instrument-block"],
        exec_args=["0"],
    ) + ["-axml", "--origins"]

    os.rename("main.srctrace", "traces/zero.srctrace")

    run_cov_program(executable=exepath_to("main"), exec_args=["1"])
    os.rename("main.srctrace", "traces/one.srctrace")

    xcov_args.remove("main.srctrace")
    return xcov_args


def test_with_checkpoint() -> None:
    xcov_args = instr_and_run_zero_one()

    # Create a checkpoint "c.ckpt"
    xcov(xcov_args + ["-Ttraces/", "--save-checkpoint=c.ckpt"])

    # Run the executable once more with argument 2. Rename the new trace to
    # "traces/two.srctrace".
    run_cov_program(executable=exepath_to("main"), exec_args=["2"])
    os.rename("main.srctrace", "traces/two.srctrace")

    # Commpute the coverage using "two.srctrace" and checkpoint "c.ckpt"
    xcov(xcov_args + ["-Ttraces/two.srctrace", "--checkpoint=c.ckpt"])

    # Check the XML report against the expected result. We only expected to
    # find "two.srctrace" or "c.ckpt" as origins.
    check_xml("with_checkpoint.xml.expected")


def test_only_traces() -> None:
    xcov_args = instr_and_run_zero_one()

    # Run the executable once more with argument 2. Rename the new trace to
    # "traces/two.srctrace".
    run_cov_program(executable=exepath_to("main"), exec_args=["2"])
    os.rename("main.srctrace", "traces/two.srctrace")

    # Commpute the coverage using the three trace files
    xcov(xcov_args + ["-Ttraces/"])

    # Check the XML report against the expected result. We only expected to
    # find "two.srctrace" or "c.ckpt" as origins.
    check_xml("only_traces.xml.expected")


# Run the tests

test_only_traces()
delete_traces()
test_with_checkpoint()
thistest.result()
