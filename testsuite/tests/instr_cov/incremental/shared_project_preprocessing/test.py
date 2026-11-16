"""
Check reuse of shared Ada instrumentation across preprocessing changes.
Cover file-specific configurations, the default, and compiler definitions.
"""

from e3.testsuite.driver.diff import Substitute

from SCOV.minicheck import xcov_instrument
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
    for i in (1, 2):
        thistest.log(f"Create preprocessing data: test{i}")
        text_to_file(
            f'"test{i}.adb" -DVALUE=value{i}\n', filename=f"prep{i}.txt"
        )
    projects = [
        GPRswitches(
            root_project=gprfor(
                prjid=f"test{i}",
                mains=[f"test{i}.adb"],
                srcdirs=[f"../test{i}"],
                objdir=f"obj_test{i}",
                deps=[shared],
                compiler_extra=(
                    f'for Switches ("test{i}.adb") use '
                    '("-gnatep=" & Project\'Project_Dir & '
                    f'"prep{i}.txt");'
                ),
            )
        )
        for i in (1, 2)
    ]

    # Different settings for the mains must not prevent sharing Pkg's
    # instrumentation between the two root projects.
    for i, project in enumerate(projects, 1):
        thistest.log(f"Initial instrumentation: test{i}")
        xcov_instrument(project, covlevel="stmt", extra_args=extra_args)
        check_instrumented(
            f"{parallel=}, test{i}",
            "instrument-all" if i == 1 else "reuse-shared",
        )

    # Body settings affect Pkg, while spec settings also affect its callers.
    definitions = {"test2.adb": "-DVALUE=value2"}
    for source, options, baseline in (
        ("test2.adb", "-DVALUE=value3", "reuse-shared"),
        ("pkg.adb", "-DVALUE=value1", "reuse-main"),
        ("pkg.adb", "-DVALUE=value2", "reuse-main"),
        ("pkg.ads", "-DA=one -DB=two", "instrument-all"),
        ("pkg.ads", "-DB=two -DA=one", "reuse-all"),
        ("pkg.ads", "-DB=three -DA=one", "instrument-all"),
    ):
        thistest.log(f"Preprocessing: {source=}, {options=}")
        definitions[source] = options
        text_to_file(
            "".join(
                f'"{filename}" {switches}\n'
                for filename, switches in definitions.items()
            ),
            filename="prep2.txt",
        )
        xcov_instrument(projects[-1], covlevel="stmt", extra_args=extra_args)
        check_instrumented(f"{parallel=}, {source=}, {options=}", baseline)

    # Only Pkg's spec and body use the default configuration. Keep the main's
    # configuration fixed so its re-instrumentation checks the spec dependency.
    for iteration, value in enumerate((1, 1, 2, 2)):
        thistest.log(f"Default preprocessing: {iteration=}, {value=}")
        text_to_file(
            f'* -DDEFAULT={value}\n"test2.adb" -DVALUE=value3\n',
            filename="prep2.txt",
        )
        xcov_instrument(projects[-1], covlevel="stmt", extra_args=extra_args)
        check_instrumented(
            f"{parallel=}, default preprocessing {iteration=}, {value=}",
            "instrument-all" if iteration in (0, 2) else "reuse-all",
        )

    # Command-line definitions must also invalidate instrumentation when
    # their values change. Repeating the same definition must reuse it.
    for iteration, (value, changed) in enumerate(
        ((1, True), (1, False), (2, True))
    ):
        thistest.log(f"Preprocessor definition: {iteration=}, {value=}")
        shared_project(
            f'for Switches ("pkg.adb") use ("-gnateDVALUE={value}");'
        )
        xcov_instrument(
            projects[-1],
            covlevel="stmt",
            extra_args=extra_args,
        )
        check_instrumented(
            f"{parallel=}, preprocessing {value=}",
            "instrument-all" if changed else "reuse-all",
        )

thistest.result()
