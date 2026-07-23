"""
Check the behavior of the --excluded-units switch: units matching one of its
patterns are removed from the set of units of interest.
"""

from SCOV.minicheck import (
    CovReport,
    build_run_and_coverage,
    check_xcov_reports,
)
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


def run_scenario(
    name: str,
    expected_cov: CovReport,
    units: list[str] | None = None,
    excluded_units: list[str] | None = None,
    gpr_extra: str | None = None,
    tolerate_messages: str | None = None,
) -> str:
    """
    Run a build/run/coverage sequence in the tmp_<name> directory, passing
    the given --units/--excluded-units arguments to gnatcov, and check that
    the xcov reports match expected_cov. Return the contents of the "gnatcov
    coverage" output.
    """
    wd = Wdir(f"tmp_{name}")
    p = gprfor(mains=["main.adb"], srcdirs=[".."], extra=gpr_extra or "")
    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=p, units=units, excluded_units=excluded_units
        ),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["--annotate=xcov", "--output-dir=xcov"],
        tolerate_instrument_messages=tolerate_messages,
        tolerate_coverage_messages=tolerate_messages,
    )
    check_xcov_reports("xcov", expected_cov)
    coverage_log = contents_of("coverage.log")
    wd.to_homedir()
    return coverage_log


main_cov = {"main.adb.xcov": {"+": {5, 6, 7}}}


def pkg_cov(*pkgs: str) -> CovReport:
    result: CovReport = {}
    for pkg in pkgs:
        result[f"{pkg}.ads.xcov"] = {}
        result[f"{pkg}.adb.xcov"] = {"+": {6}}
    return result


# Excluding pkg2 on the command line must leave all the other units of
# interest, and override the Excluded_Units attribute (i.e. pkg1 must be of
# interest even though the attribute excludes it).
run_scenario(
    name="override",
    expected_cov={**main_cov, **pkg_cov("pkg1", "pkg3")},
    excluded_units=["pkg2"],
    gpr_extra=(
        "package Coverage is\n"
        '   for Excluded_Units use ("pkg1");\n'
        "end Coverage;"
    ),
)

# --excluded-units must combine with --units: the set of units of interest is
# the set of units matching --units minus the ones matching --excluded-units.
run_scenario(
    name="with_units",
    expected_cov=pkg_cov("pkg1", "pkg3"),
    units=["pkg*"],
    excluded_units=["pkg2"],
)

# A pattern that matches no unit must not exclude anything and must trigger a
# warning.
coverage_log = run_scenario(
    name="no_match",
    expected_cov={**main_cov, **pkg_cov("pkg1", "pkg2", "pkg3")},
    excluded_units=["no_such_unit"],
    tolerate_messages="no unit no_such_unit",
)
thistest.fail_if_no_match(
    "gnatcov coverage output",
    r"(?s).*no unit no_such_unit \(from --excluded-units\) in the projects"
    " of interest.*",
    coverage_log,
)

thistest.result()
