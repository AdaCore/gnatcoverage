"""
Test that there are no issues when the LAL context is recreated when querying a
unit during the search of manual dump/reset indications.
Two bugs used to make this crash:
 - The JSON configuration pragma file used to be deleted after finishing the
   coverage instrumentation, thus crashing gnatcov when a LAL context was
   recreated.
 - The manual dump search first performs a search of the relevant pragmas on
   all sources, and only starts a rewriting session once one such pragma is
   found. This results in two calls to get_from_file for the same unit, which
   leads to stale references once the rewriting sessions was initialized.
This regression test reproduces both bugs.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches, gprcov_for
from SUITE.tutils import gprfor


tmp = Wdir ("tmp_")

# Number of LAL unit queries after which gnatcov reset the LAL context
GNATCOV_CTX_RESET_LIMIT = 50

def gen_project():
    """
    Generate a project capable of reproducing the bug.
    We need to create exactly 50 sources to instrument for coverage (the limit
    at which gnatcov resets its LAL context) then inspect at least one source
    in search of a manual buffer dump indication.
    Returns a tuple consisting in the generated project name (by gprfor) and
    the expected coverage result.
    """

    # First, generate the sources to be instrumented. They'll only consist in
    # the bare minimum to generate an obligation: a simple variable
    # declaration.
    for i in range(GNATCOV_CTX_RESET_LIMIT):
        with open(f"pkg_{i}.ads", "w") as pkg_file:
            pkg_file.write(f"package Pkg_{i} is\n")
            pkg_file.write("   Dummy_Bool : Boolean := False;\n")
            pkg_file.write(f"end Pkg_{i};\n")

    # Then, generate a main that pulls all these units in the compilation
    # closure, and includes a manual dump indication.

    with open("main.adb", "w") as main_file:
        for i in range(GNATCOV_CTX_RESET_LIMIT):
            main_file.write(f"with Pkg_{i};\n")
        main_file.write("procedure Main is\n")
        main_file.write("begin\n")
        main_file.write("   null;\n")
        main_file.write("   pragma Annotate (Xcov, Dump_Buffers);\n")
        main_file.write("end Main;\n")

    # Create a project. The Pkg_{i} sources are of interest for coverage
    # purposes, but the main not.
    prj = gprfor(
        mains=["main.adb"],
        prjid="prj",
        srcdirs=".",
        objdir="obj",
        extra=gprcov_for(
            units_in=[f"Pkg_{i}" for i in range(GNATCOV_CTX_RESET_LIMIT)]
        )
    )
    expected_cov={
        f"pkg_{i}.ads.xcov": {'+': {2}} for i in range(GNATCOV_CTX_RESET_LIMIT)
    }
    return prj, expected_cov


# First, generate a project

prj, expected_cov = gen_project()

# Then instrument, build, run, coverage and check
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    mains=["main"],
    covlevel="stmt",
    extra_coverage_args=["-axcov"],
    dump_trigger="manual",
    manual_prj_name=os.path.splitext(prj)[0]
)

check_xcov_reports("*.xcov", expected_cov, cwd="obj")

thistest.result()
