"""
Check that passing an explicit list of mains to "gnatcov instrument"'s command
line works as expected.
"""

import glob
import re

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, srctracename_for
from SUITE.gprutils import GPRswitches


def create_gprsw(mains, with_asm=False):
    """
    Generate the test project in the current directory (must be the temporary
    directory) and return the corresponding GPRswitches instance.

    :param mains: List of source files for the mains.
    :param with_asm: Whether to include "Asm" in the list of languages.
    """
    # First create a GPR file for the library to test. Make it standalone and
    # auto initialized so that buffer units gets linked even if a main contains
    # no buffer dumps and thus does not pull it into the link.
    gprfor(
        mains=[],
        prjid="mylib",
        srcdirs="../mylib",
        objdir="obj-mylib",
        langs=["Ada"],
        extra="""
            for Library_Name use "mylib";
            for Library_Dir use "lib";
            for Library_Interface use ("pkg");
            for Library_Standalone use "standard";
        """,
    )

    # Then create the GPR file for the mains
    langs = ["Ada"]
    if with_asm:
        langs.append("Asm")
    mains_gpr = gprfor(
        mains=mains,
        prjid="mains",
        srcdirs="..",
        langs=langs,
        deps=["mylib"],
    )

    # Create object/library directories to avoid GPR warnings that they are
    # missing.
    mkdir("obj-mylib")
    mkdir("lib")
    mkdir("obj")

    return GPRswitches(root_project=mains_gpr, projects=["mylib"])


# First, test error cases

wd = Wdir("tmp_errors")

gprsw = create_gprsw(mains=[], with_asm=True)


def check_error(label, mains, expected_error_msg):
    """
    Run a test variant to check an error case.

    :param label: Name for this test variant.
    :param mains: List of mains to pass on the command line of
        "gnatcov instrument".
    :param expected_error_msg: Expected error message from
        "gnatcov instrument".
    """
    thistest.log(f"== {label} ==")
    log_file = f"output-{label}.txt"
    p = xcov_instrument(
        gprsw=gprsw,
        covlevel="stmt",
        extra_args=mains,
        out=log_file,
        register_failure=False,
    )
    thistest.fail_if_not_equal("gnatcov instrument status code", 1, p.status)
    thistest.fail_if_no_match(
        "gnatcov instrument output",
        f".*gnatcov.*: {re.escape(expected_error_msg)}",
        contents_of(log_file).strip(),
    )


check_error(
    "nosuchfile", ["nosuchfile.adb"], "No such source file: nosuchfile.adb"
)
check_error(
    "badlang",
    ["foo.s"],
    "Cannot instrument main source file (unsupported language): foo.s",
)

wd.to_homedir()


# Now, check that expected mains are instrumented for each cases: no explicit
# main on the command line, one or several explicit mains. Go through the whole
# instrument/build/run cycle and check the absence or presence of a trace file
# in each case.


def check_gen_traces(
    label, gpr_mains, xcov_mains, program_for_expected_traces
):
    """
    Run a test variant to check mains with coverage buffer dumps.

    :param label: Name for this test variant.
    :param gpr_mains: List of source files to consider as mains in the project
        file.
    :param xcov_mains: List of source files to pass as mains to
        "gnatcov instrument".
    :param program_for_expected_traces: List of program names for which we
        expect a source trace to be produced.
    """
    thistest.log(f"== {label} ==")
    wd = Wdir(f"tmp_{label}")

    build_and_run(
        gprsw=create_gprsw(gpr_mains),
        covlevel="stmt",
        # Build and run all actual mains in all cases (we want to check the
        # instrumentation status for all of them).
        extra_gprbuild_args=["main1.adb", "main2.adb"],
        mains=["main1", "main2"],
        extra_instr_args=xcov_mains,
        extra_coverage_args=[],
        trace_mode="src",
        # Since instrumentation may insert coverage buffer dumps only for a
        # subset of mains, calls to "gnatcov extract-base64-trace" will fail
        # for mains that do not dump. This is as expected, so ignore these
        # errors.
        register_failure=False,
    )

    # Check that we get exactly the traces that we were expecting
    expected_traces = sorted(
        srctracename_for(program) for program in program_for_expected_traces
    )
    actual_traces = sorted(glob.glob("*.srctrace"))
    thistest.fail_if_not_equal(
        "Source trace files", expected_traces, actual_traces
    )

    wd.to_homedir()


# No mains defined in GPR nor on the gnatcov cmdline: no main creates a trace
# file.
check_gen_traces("gpr0-xcov0", [], [], [])

# Only the GPR file declares a main: only that main creates a trace file
check_gen_traces("gpr1-xcov0", ["main1.adb"], [], ["main1"])

# The GPR file declares one main, but the gnatcov cmdline declares another
# main: the latter takes precedence.
check_gen_traces("gpr1-xcov2", ["main1.adb"], ["main2.adb"], ["main2"])

# Likewise, but the gnatcov cmdline declares two mains
check_gen_traces(
    "gpr1-xcov12",
    ["main1.adb"],
    ["main1.adb", "main2.adb"],
    ["main1", "main2"],
)

# The GPR file declares no main, but the gnatcov cmdline declares one: the
# latter creates a trace file.
check_gen_traces("gpr0-xcov1", [], ["main1.adb"], ["main1"])

thistest.result()
