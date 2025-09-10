"""
Check that warnings about projects not contributing to the selection of units
of interest are emitted when expected.
"""

import os.path
import glob

from e3.fs import mkdir

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches, gprcov_for
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")


class ProjectConfig(object):
    """Helper to generate a project file.

    Instances hold information about the units of interest information that the
    Coverage package must contain, and the rest (project name, dependencies and
    mains) is determined during the generation (see the generate method below).
    """

    def __init__(self, units_in=None, units_out=None):
        self.units_in = units_in
        self.units_out = units_out

    def generate(self, name, deps=(), mains=()):
        return gprfor(
            prjid=name,
            mains=mains,
            deps=deps,
            srcdirs=["../../src-{}".format(name)],
            objdir="obj-{}".format(name),
            extra=gprcov_for(units_in=self.units_in, units_out=self.units_out),
        )


def run_test(
    label,
    slug,
    main,
    helper,
    recursive,
    projects=None,
    units=None,
    projects_warned=None,
    expected_cov_list=None,
):
    """
    Produce a coverage report for the given parameters and check the emitted
    warnings.

    :param str label: Label for this test.
    :param str slug: Unique short string for this test (used to create
        directories).
    :param ProjectConfig main: Configuration for the "main" project.
    :param ProjectConfig helper: Configuration for the "helper" project.
    :param bool recursive: Whether to not pass --no-subprojects.
    :param list[str] | None projects: List of projects to pass with --projects.
    :param list[str] | None units: List of units to pass with --units.
    :param list[str] | None projects_warned: List of projects for which we
        expected warnings.
    :param expected_cov: List of expected coverage reports.
    """
    projects = projects or []
    units = units or []
    projects_warned = projects_warned or []
    expected_cov_list = expected_cov_list or []

    thistest.log("== [{}] {} ==".format(slug, label))
    tmp.to_subdir("tmp_/{}".format(slug))

    expected_output = "\n".join(
        "warning: project {} provides no unit of interest".format(project)
        for project in projects_warned
    )

    # Generate projects for this test (see below for the description of each
    # project).
    ProjectConfig().generate("empty")
    helper.generate("helper")
    main_prj = main.generate(
        "main", deps=["empty", "helper"], mains=["main.adb"]
    )
    mkdir("obj-empty")
    mkdir("obj-helper")
    mkdir("obj-main")

    # Generate a coverage report for them
    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=main_prj,
            projects=projects,
            units=units,
            no_subprojects=not recursive,
        ),
        covlevel="stmt",
        mains=["main"],
        gpr_obj_dir="obj-main",
        tolerate_instrument_messages="project .* provides no unit",
        extra_coverage_args=["-axcov"],
        tolerate_coverage_messages="project .* provides no unit",
    )

    log_file = (
        "coverage.log"
        if thistest.options.trace_mode == "bin"
        else "instrument.log"
    )
    thistest.fail_if_not_equal(
        "[{}/{}] gnatcov output".format(label, slug),
        expected_output,
        contents_of(log_file).strip(),
    )

    expected_cov = {}
    if expected_cov_list:
        for c in expected_cov_list:
            expected_cov.update(c)
    check_xcov_reports("obj-main", expected_cov)

    # Check that all xcov report files are created in obj-main (i.e. the root
    # project).
    xcov_files = glob.glob("obj-*/*.xcov")
    extra_xcov_files = [
        f for f in xcov_files if os.path.dirname(f) != "obj-main"
    ]
    thistest.fail_if_not_equal(
        "misplaced xcov report files",
        "",
        "\n".join(extra_xcov_files),
    )


# For all testcases, we set up three projects:
#
# * "main", to contain the "main" and main_support units, depends on the
#   "helper" project;
#
# * "helper", to contain the "helper" and helper_support units, depends on the
#   "empty" project;
#
# * "empty", to contain no unit and no dependency. Note that gnatcov never
#   warns about it in the tests below because "empty" is never passed as a
#   project of interest (--projects) unless not recursive (no warnings
#   are expected in this mode). The point of having this project loaded is to
#   check that indeed gnatcov does not warn about "empty".
main = {"main.adb.xcov": {"+": {6, 7}}}
main_support = {"main_support.adb.xcov": {"+": {3}}}
helper = {"helper.adb.xcov": {"+": {5}}}
helper_support = {"helper_support.adb.xcov": {"+": {3}}}


# Check when --units is passed

# Pass both projects to --projects and only "main" to --units so that the
# "helper" project has no unit of interest: gnatcov must warn about it.
run_test(
    label="--units present, no UoI in helper",
    slug="up1",
    main=ProjectConfig(),
    helper=ProjectConfig(),
    projects=["main", "helper"],
    units=["main"],
    projects_warned=["helper"],
    expected_cov_list=[main],
    recursive=False,
)

# Pass both projects to --projects and both "main" and "h[ea]lper*" to --units.
# The globbing pattern should match helper and helper_support. Both projects
# have at least one unit of interest, so we expect no warning.
run_test(
    label="--units present, all projects have UoI",
    slug="up2",
    main=ProjectConfig(),
    helper=ProjectConfig(),
    projects=["main", "helper"],
    units=["main", "h[ea]lper*"],
    projects_warned=[],
    expected_cov_list=[main, helper, helper_support],
    recursive=False,
)

# Only pass the root project (-P) in recursive mode and pass
# "main" to --units. Even though "helper" is a project of interest, it does not
# contain any unit of interest, yet we expect no warning in recursive mode.
run_test(
    label="--units present, no UoI in helper, recursive",
    slug="up3",
    main=ProjectConfig(),
    helper=ProjectConfig(),
    units=["main"],
    projects_warned=[],
    expected_cov_list=[main],
    recursive=True,
)


# Check when we don't pass --units

# Pass both projects to --projects, no --units argument and make "helper" have
# an empty Coverage'Units attribute. "helper" has no unit of interest, but it
# has an attribute, se we should not warn about it.
run_test(
    label="--units absent, no UoI in helper with Units",
    slug="ua1",
    main=ProjectConfig(),
    helper=ProjectConfig(units_in=[]),
    projects=["main", "helper"],
    projects_warned=[],
    expected_cov_list=[main, main_support],
    recursive=False,
)

# Pass both projects to --projects, no --units argument and make "helper" have
# one unit pattern in its Coverage'Units attribute. All projects have units of
# interest, so no warning expected.
run_test(
    label="--units absent, no UoI in helper with Excluded_Units",
    slug="ua2",
    main=ProjectConfig(),
    helper=ProjectConfig(units_out=["helper*"]),
    projects=["main", "helper"],
    projects_warned=[],
    expected_cov_list=[main, main_support],
    recursive=False,
)

# Pass both projects to --projects in recursive mode, no --units
# argument. All projects have units of interest and we are in recursive mode
# anyway, so no warning expected.
run_test(
    label="--units absent, no UoI in helper, recursive",
    slug="ua3",
    main=ProjectConfig(),
    helper=ProjectConfig(),
    projects_warned=[],
    expected_cov_list=[main, main_support, helper, helper_support],
    recursive=True,
)


thistest.result()
