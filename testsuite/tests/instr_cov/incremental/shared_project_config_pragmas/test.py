"""
Check reuse of shared Ada instrumentation across configuration pragma changes.
Cover local and global pragma contents and their assignment to source files.
"""

from e3.testsuite.driver.diff import Substitute

from SCOV.minicheck import (
    build_and_run,
    check_xcov_reports,
    xcov,
    xcov_instrument,
)
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, text_to_file
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir()
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")


def shared_project(compiler_extra: str = "") -> str:
    return gprfor(
        prjid="shared",
        mains=[],
        srcdirs=["../shared"],
        objdir="obj_shared",
        compiler_extra=compiler_extra,
    )


def check_instrumented(label: str, baseline: str) -> None:
    # Scheduler iteration counts differ between execution modes.
    thistest.fail_if_diff(
        baseline_file=f"../{baseline}.expected",
        actual_file="instrument.log",
        failure_message=label,
        output_refiners=thistest.report_output_refiners()
        + [
            Substitute(
                "[GPR.ACTIONS_SCHEDULER] Starting the graph iterator\n"
            ),
            Substitute(
                "[GPR.ACTIONS_SCHEDULER] End of iteration: "
                "status /= pending and no more nodes available\n"
            ),
        ],
    )


for parallel in (False, True):
    thistest.log(f"== {parallel=} ==")
    tmp.to_subdir("tmp_parallel" if parallel else "tmp_serial")
    extra_args = ["--force-parallelism"] if parallel else []
    shared = shared_project()
    projects = [
        GPRswitches(
            root_project=gprfor(
                prjid=f"test{i}",
                mains=[f"test{i}.adb"],
                srcdirs=[f"../test{i}"],
                objdir=f"obj_test{i}",
                deps=[shared],
            )
        )
        for i in (1, 2)
    ]

    # Instrument, build and run both projects a first time. We expect the
    # second project to not reinstrument shared sources.
    traces = []
    for i, project in enumerate(projects, 1):
        thistest.log(f"Build and run: test{i}")
        coverage_args = build_and_run(
            gprsw=project,
            covlevel="stmt",
            mains=[f"test{i}"],
            gpr_obj_dir=f"obj_test{i}",
            extra_coverage_args=[],
            extra_instr_args=extra_args,
        )
        check_instrumented(
            f"{parallel=}, test{i}",
            "instrument-all" if i == 1 else "reuse-shared",
        )
        traces.append(coverage_args[-1])

    # Sanity check that both runs produce usable traces for the shared unit.
    # The instrument.log comparisons above check incrementality.
    thistest.log("Check coverage of the shared unit")
    xcov(
        [
            "coverage",
            "--level=stmt",
            "--annotate=xcov",
            "--projects=shared",
        ]
        + projects[0].cov_switches
        + traces
    )
    check_xcov_reports(
        "obj_test1",
        {
            "pkg.ads.xcov": {},
            "pkg.adb.xcov": {"+": {4}},
        },
    )

    # Re-instrument the second project several times, sometimes changing the
    # local configuration pragmas: when they change, only the Pkg unit should
    # be reinstrumented.
    previous_policy = None
    for iteration, policy in enumerate(
        (None, "Check", "Check", "Ignore", None)
    ):
        thistest.log(f"Local pragmas: {iteration=}, {policy=}")
        if policy:
            text_to_file(
                f"pragma Assertion_Policy ({policy});\n", filename="config.adc"
            )
        shared_project(
            'for Local_Configuration_Pragmas use "config.adc";'
            if policy
            else ""
        )
        xcov_instrument(
            projects[-1],
            covlevel="stmt",
            extra_args=extra_args,
        )
        changed = policy != previous_policy
        check_instrumented(
            f"{parallel=}, local pragmas {policy=}",
            "reuse-main" if changed else "reuse-all",
        )
        previous_policy = policy

    gprfor(
        prjid="test2",
        mains=["test2.adb"],
        srcdirs=["../test2"],
        objdir="obj_test2",
        deps=[shared],
        extra="""
            package Builder is
               for Global_Configuration_Pragmas use
                 external ("GLOBAL_CONFIG", "global.adc");
            end Builder;
        """,
    )
    # Global configuration pragma changes must reinstrument both units.
    previous_policy = None
    for iteration, policy in enumerate(("Check", "Ignore", "Ignore")):
        thistest.log(f"Global pragmas: {iteration=}, {policy=}")
        text_to_file(
            f"pragma Assertion_Policy ({policy});\n", filename="global.adc"
        )
        xcov_instrument(
            projects[-1],
            covlevel="stmt",
            extra_args=extra_args,
        )
        check_instrumented(
            f"{parallel=}, global pragmas {policy=}",
            "instrument-all" if policy != previous_policy else "reuse-all",
        )
        previous_policy = policy

    # Swap the global and local pragma files. Both files remain inputs, but
    # changing which one applies globally must invalidate instrumentation.
    text_to_file("pragma Assertion_Policy (Check);\n", filename="config.adc")
    for global_file, local_file, baseline in (
        ("global.adc", "config.adc", "reuse-main"),
        ("config.adc", "global.adc", "instrument-all"),
    ):
        thistest.log(f"Pragma files: {global_file=}, {local_file=}")
        shared_project(f'for Local_Configuration_Pragmas use "{local_file}";')
        xcov_instrument(
            projects[-1],
            covlevel="stmt",
            extra_args=extra_args + [f"-XGLOBAL_CONFIG={global_file}"],
        )
        check_instrumented(
            f"{parallel=}, {global_file=}, {local_file=}", baseline
        )


thistest.result()
