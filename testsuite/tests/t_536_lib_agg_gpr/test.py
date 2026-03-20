"""
Test that the selection of project/unit of interest works as expected for
aggregate library projects.
"""

from e3.fs import cp

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


cov_reports = {
    "main.adb": {"+": {5, 6}},
    "pkg_a.ads": {"+": {2}},
    "pkg_b.ads": {"+": {2}},
}


def check(
    label: str,
    cov_files: list[str],
    projects: list[str] | None = None,
    units: list[str] | None = None,
    no_subprojects: bool = False,
    expect_failure: bool = False,
) -> None:
    """
    :param label: Slug to identify this check.
    :param cov_files: Set of file basenames expected in the coverage report.
    :param projects: List of project names to pass to gnatcov --projects.
    :param units: List of project names to pass to gnatcov --units.
    :param no_subprojects: Whether to pass --no-subprojects to gnatcov.
    :param expect_failure: Whether "gnatcov instrument" is expected to fail.
    """
    thistest.log(f"== {label} ==")
    tmp = Wdir(f"tmp_{label}")

    cp("../*.ads", ".")
    cp("../*.gpr", ".")
    main_gpr = gprfor(
        prjid="main",
        srcdirs=[".."],
        deps=["agg"],
        mains=["main.adb"],
        extra='for Source_Files use ("main.adb");',
    )

    gprsw = GPRswitches(
        root_project=main_gpr,
        projects=projects,
        units=units,
        no_subprojects=no_subprojects,
    )

    if expect_failure:
        p = xcov_instrument(
            gprsw=gprsw,
            covlevel="stmt",
            register_failure=False,
        )
        thistest.fail_if(
            p.status == 0, '"gnatcov instrument" failure expected'
        )
    else:
        build_run_and_coverage(
            gprsw=gprsw,
            covlevel="stmt",
            mains=["main"],
            extra_coverage_args=["-axcov", "--output-dir=xcov"],
            trace_mode="src",
        )
        check_xcov_reports(
            "xcov", {f"{file}.xcov": cov_reports[file] for file in cov_files}
        )

    tmp.to_homedir()


check("all", ["main.adb", "pkg_a.ads", "pkg_b.ads"])

check("prj_rec_a", ["pkg_a.ads"], projects=["a"])
check("prj_rec_agg", ["pkg_a.ads", "pkg_b.ads"], projects=["agg"])

check("prj_a", ["pkg_a.ads"], no_subprojects=True, projects=["a"])
check(
    "prj_agg", [], no_subprojects=True, projects=["agg"], expect_failure=True
)

check("unit_a", ["pkg_a.ads"], units=["pkg_a"])

thistest.result()
