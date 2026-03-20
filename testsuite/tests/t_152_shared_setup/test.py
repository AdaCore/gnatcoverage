"""
Check that "gnatcov setup" does not try to build a shared version of the
coverage runtime when the Ada runtime is not available as a shared library.

This is the case for all of our light runtimes: even though the platform might
support them, we generally don't build the shared version of the runtime.
"""

import os

from SCOV.instr import xcov_convert_base64, xcov_instrument
from SCOV.minicheck import check_xcov_reports
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import (
    exepath_to,
    gprfor,
    run_and_log,
    run_cov_program,
    xcov,
)
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Name of the installed runtime project, to avoid conflict with the one setup
# by the testing infrastructure.
rt_prj = "rt_prj"

# Installation directory for the instrumentation runtime project and directory
# for the project path.
rt_install_dir = os.path.abspath("install")
rt_path_dir = os.path.join(rt_install_dir, "share", "gpr")

prj = gprfor(
    mains=["main.adb"],
    objdir="obj",
    srcdirs=[".."],
    extra='   for Runtime ("Ada") use "light";',
)

gprsw = GPRswitches(root_project=prj)

light_log = "setup_light.log"

# Setup the coverage runtime for a light Ada runtime. Disable the automatic
# passing of --config / --target / --rts as we are doing something custom.
xcov(
    [
        "setup",
        "--RTS=light",
        f"--install-name={rt_prj}",
        f"--prefix={rt_install_dir}",
        "-v",
    ],
    out=light_log,
    auto_target_args=False,
    auto_config_args=False,
)

thistest.fail_if(
    "Library support: STATIC_ONLY" not in contents_of(light_log),
    "Incorrect library support for light runtime",
)

# Make this install available to GPR tools
env.add_search_path("GPR_PROJECT_PATH", rt_path_dir)

# As we want to override the config generated for the testsuite, we have to
# manually re-do all the coverage workflow to ensure no "--config" argument is
# used in the process.

xcov_instrument(
    gprsw,
    covlevel="stmt",
    dump_channel="base64-stdout",
    dump_trigger="main-end",
    runtime_project=rt_prj,
    auto_config_args=False,
    auto_target_args=False,
)

build_log = "gprbuild.log"

p = run_and_log(
    cmds=[
        "gprbuild",
        f"-P{prj}",
        f"--implicit-with={rt_prj}",
        "--src-subdirs=gnatcov-instr",
    ],
    output=build_log,
    env=env.environ,
)

thistest.fail_if(p.status != 0, "GPRbuild exit in error")

run_log = "run.log"

# Ignore exit status as there's no way with a light runtime to set the exit
# status to 0.
run_cov_program(
    exepath_to("main"),
    out=run_log,
    register_failure=False,
)

main_srctrace = "main.srctrace"

xcov_convert_base64(run_log, main_srctrace)

xcov(
    [
        "coverage",
        f"-P{prj}",
        "-cstmt",
        "-axcov",
        main_srctrace,
    ]
)

check_xcov_reports("obj", {"main.adb.xcov": {"+": {5}}})

thistest.result()
