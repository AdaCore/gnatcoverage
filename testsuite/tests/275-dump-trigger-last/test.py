"""
Test that "gnatcov instrument" only uses the last --dump-trigger option
present on the command line or project file. This was no longer the case when
--dump-trigger=manual started accepting files to be searched as a comma
separated string.
"""

import glob

from SCOV.instr import default_dump_trigger
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches, gprcov_for, Csw
from SUITE.tutils import gprfor

expected_cov = {"main.adb.xcov": {"+": {5, 7}}}

tmp = Wdir()


def check_one(tc_id, extra_instr_args=None, extra_prj_content=""):
    """
    Run a build, run, coverage and report-check workflow. The expected coverage
    shall always be expected_cov.

    tc_prefix is used to differentiate artefact names from each invocation,
    namely the temporary directory name.

    extra_instr_args is forwarded to build_and_run's homonym parameter, note
    that if it is left empty, no --dump-trigger switch will be passed to the
    gnatcov instrument invocation.

    extra_prj_content is passed to gprfor's extra parameter if not None.
    """

    tmp.to_subdir(f"tmp_{tc_id}")
    thistest.log(f"========= {tc_id} =========")
    prj = gprfor(
        mains=["main.adb"],
        srcdirs=[".."],
        extra=extra_prj_content,
    )
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=prj),
        covlevel="stmt",
        dump_trigger=None,
        mains=["main"],
        extra_instr_args=extra_instr_args,
        extra_coverage_args=["-axcov"],
    )
    check_xcov_reports("obj", expected_cov)

    # Check that a single trace was created
    nb_traces = len(glob.glob("*.srctrace"))
    thistest.fail_if_not_equal(
        "Unexpected number of traces", expected=1, actual=nb_traces
    )


# Auto dump trigger to be used in the test
dump_trigger = default_dump_trigger(["main.adb"])

# Test that with two arguments on the command line, only the second one is
# taken into account.
check_one(
    "cmd_line",
    extra_instr_args=[
        "--dump-trigger=manual",
        f"--dump-trigger={dump_trigger}",
    ],
)

# Test that if there is a dump trigger indication in the project file, and on
# the command line, the later will be used.
check_one(
    "prj_and_cmd",
    extra_instr_args=[f"--dump-trigger={dump_trigger}"],
    extra_prj_content=gprcov_for(
        switches=[Csw("instrument", ["--dump-trigger=manual"])]
    ),
)

# Test that if there are multiple occurrences of the --dump-trigger switch in
# the project Switches attribute, only the last one is used.
check_one(
    "prj_switches",
    extra_prj_content=gprcov_for(
        switches=[
            Csw(
                "instrument",
                ["--dump-trigger=manual", f"--dump-trigger={dump_trigger}"],
            )
        ]
    ),
)

thistest.result()
