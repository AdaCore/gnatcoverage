"""
Check that GNATcoverage does not process externally built projects, unless
--externally-built-projects is passed.
"""

import os.path
import re
import shutil

from e3.fs import cp

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprfor, gprinstall, xcov


# Dump triggers to be tested. The default one for the platform and manual for
# source traces, irrelevant for bin traces, we simply need one to run the
# test.
#
# TODO the test seems to be dead for bin traces, can we simplify this???
dump_triggers = (
    ["manual", "auto"]
    if thistest.options.trace_mode == "src"
    else ["bin-trace"]
)


def check_coverage(
    project, externally_built, expected_cov, trace_file, register_failure=True
):
    """
    Create a coverage report for project from trace_file,
    requesting coverage for externally-built projects based on
    externally_built, and check the report against expected_cov.
    Return the process handle and the output filename for the
    "gnatcov coverage" command and ignore its exit status if
    register_failure is False.
    """
    output_dir = (
        f"xcov_{os.path.basename(project[:-4])}_"
        f"{'ext' if externally_built else 'no_ext'}"
    )
    thistest.log(f"coverage in {output_dir}")
    gnatcov_log = f"gnatcov_{output_dir}.log"
    if os.path.isdir(output_dir):
        shutil.rmtree(output_dir)
    os.mkdir(output_dir)

    args = [
        "coverage",
        "-cstmt",
        "-axcov",
        "-P",
        project,
        "--output-dir",
        output_dir,
        trace_file,
    ]
    if externally_built:
        args.append("--externally-built-projects")
    p = xcov(args, out=gnatcov_log, register_failure=register_failure)

    if not register_failure:
        return p, gnatcov_log

    check_xcov_reports(output_dir, expected_cov)


def check_one(dump_trigger, lib_prj):
    """
    Build and run the main project, inspecting externally built projects,
    then check all four combinations of project/--externally-built-projects
    to check the behaviors and results produced by gnatcov, for a given
    dump trigger (if applicable) and installed library project.
    """
    tmp.to_subdir(f"tmp_{dump_trigger}")

    # Generate a project file for this subdir
    obj_main = os.path.abspath("obj_main")
    main_gpr = gprfor(
        prjid="main",
        mains=["main.adb"],
        srcdirs=["../main"],
        objdir=obj_main,
        exedir=obj_main,
        deps=[lib_prj],
    )
    thistest.log(f"====== Testing {dump_trigger} =======")
    # Build the main project using this and run it to produce a trace file
    xcov_args = build_and_run(
        gprsw=GPRswitches(
            root_project=main_gpr,
            externally_built_projects=True,
        ),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["-axcov"],
        gpr_obj_dir=obj_main,
        gpr_exe_dir=obj_main,
        gprsw_for_coverage=False,
        dump_trigger=dump_trigger,
        manual_prj_name="main",
    )
    trace_file = xcov_args[-1]

    # Check that the instrumenter hasn't tried to instrument the installed
    # project
    thistest.fail_if(
        os.path.exists(
            os.path.join(install_dir, "share", "gpr", "mylib-gnatcov-instr")
        ),
        '"gnatcov instrument" instrumented the installed project',
    )

    # First, make sure that "gnatcov coverage" on an externally built project
    # returns an error.
    p, log_filename = check_coverage(
        project=installed_mylib_gpr,
        externally_built=False,
        expected_cov=None,
        trace_file=trace_file,
        register_failure=False,
    )
    thistest.fail_if(
        p.status == 0,
        "gnatcov was supposed to complain when provided an externally built"
        " project",
    )
    coverage_log = contents_of(log_filename).strip()
    expected_log = re.compile(
        # Regexp to accommodate output differences between the various
        # supported platforms.
        "[^\n]*gnatcov[^\n]*: {}".format(
            re.escape(
                "Root project is marked as externally built, while externally"
                " built projects are ignored by default. Consider using"
                " --externally-built-projects."
            )
        )
    )
    thistest.fail_if(
        not expected_log.match(coverage_log),
        'Unexpected output for "gnatcov coverage". Expected:\n'
        "{}\n"
        "but got:\n"
        "{}".format(indent(expected_log.pattern), indent(coverage_log)),
    )

    # It should not complain with --externally-built-projects
    p = check_coverage(
        project="mylib.gpr",
        externally_built=True,
        trace_file=trace_file,
        expected_cov={"mylib.adb.xcov": {"+": {5, 6}, "-": {8}}},
    )

    # Make sure coverage computation gives the expected result with and
    # without --externally-built-projects
    check_coverage(
        project=main_gpr,
        externally_built=False,
        trace_file=trace_file,
        expected_cov={"main.adb.xcov": {"+": {4, 6}}},
    )
    check_coverage(
        project=main_gpr,
        externally_built=True,
        trace_file=trace_file,
        expected_cov={
            "main.adb.xcov": {"+": {4, 6}},
            "mylib.adb.xcov": {"+": {5, 6}, "-": {8}},
        },
    )


tmp = Wdir("tmp_")

# Copy the library sources in the working directory
cp("../mylib", ".", recursive=True)

mylib_obj_dir = "obj_lib"
mylib_gpr = os.path.join("mylib", "mylib.gpr")

# Create the installation directory and add it to gprbuild's project lookup
# path.
install_dir = os.path.abspath("install")
gpr_install_dir = os.path.join(install_dir, "share", "gpr")
installed_mylib_gpr = os.path.join(gpr_install_dir, "mylib.gpr")
os.mkdir(install_dir)
env.add_search_path("GPR_PROJECT_PATH", gpr_install_dir)

# Build and install the library project
if thistest.options.trace_mode == "src":
    xcov_instrument(
        gprsw=GPRswitches(root_project=mylib_gpr),
        covlevel="stmt",
        gpr_obj_dir=mylib_obj_dir,
    )
gprbuild(mylib_gpr)

if thistest.options.trace_mode == "src":
    gprinstall(
        mylib_gpr,
        [
            f"--prefix={install_dir}",
            "--src-subdirs=gnatcov-instr",
            "--implicit-with=gnatcov_rts",
        ],
    )
else:
    gprinstall(mylib_gpr, f"--prefix={install_dir}")

# Run the coverage workflow for each dump trigger
for dump_trigger in dump_triggers:
    check_one(dump_trigger, installed_mylib_gpr)

thistest.result()
