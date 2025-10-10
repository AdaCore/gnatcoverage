"""
Check that dump options passed to "gnatcov setup" are used as defaults for the
subsequent "gnatcov instrument" using the installed instrumentation runtime.

Note that for test simplicity, here we only check this for --dump-channel: dump
config code being shared in gnatcov, in principle it should work for all other
--dump-* options (there are lots of such options and checking that they are
used is not trivial).
"""

import json
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Installation directory for the instrumentation runtime projects and directory
# for the project path.
rt_install_dir = os.path.abspath("install")
rt_path_dir = os.path.join(rt_install_dir, "share", "gpr")

# Make this install available to GPR tools
env.add_search_path("GPR_PROJECT_PATH", rt_path_dir)


def xsetup(install_name, args):
    """
    "xcov" wrapper to run "gnatcov setup".

    This wrapper just automatically passes --install-name and --prefix
    arguments.
    """
    xcov(
        [
            "setup",
            f"--install-name={install_name}",
            f"--prefix={rt_install_dir}",
        ]
        + args,
        force_project_args=True,
    )


# Test project to instrument
prj = gprfor(srcdirs=[".."], mains=["foo.adb"])

# Install two runtime projects, each with different dump-channel defaults. The
# default is --dump-channel=bin-file.
for rt_name, dump_args in [
    ("rt_def", []),
    ("rt_b64", ["--dump-channel=base64-stdout"]),
]:
    xsetup(rt_name, dump_args)

# Now instrument and build the test program in various scenarios. For each
# case, we just call build_run_and_coverage, which relies on the metadata that
# "gnatcov instrument" writes to the object directory in order to know how to
# read traces ("*.srctrace" binary file or decode base64 trace from the
# stdout).
#
# We expect the dump channel to be the first parameter in the following list
# (when passed):
#
# * --dump-channel passed to "gnatcov instrument";
# * --dump-channel passed to "gnatcov setup";
# * bin-file (the ultimate default for native targets).
for label, rt_name, dump_args, expected_dump_channel in [
    ("def_def", "rt_def", [], "bin-file"),
    (
        "def_base64",
        "rt_def",
        ["--dump-channel=base64-stdout"],
        "base64-stdout",
    ),
    ("b64_def", "rt_b64", [], "base64-stdout"),
    ("b64_binfile", "rt_b64", ["--dump-channel=bin-file"], "bin-file"),
]:
    thistest.log(f"== {label} ==")
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=prj),
        covlevel="stmt",
        mains=["foo"],
        extra_instr_args=dump_args + ["--runtime-project", rt_name],
        runtime_project=rt_name,
        extra_coverage_args=["-axcov", "--output-dir=."],
        dump_channel=None,
        trace_mode="src",
    )
    check_xcov_reports(".", {"foo.adb.xcov": {"+": {5}}})

    # Make sure that the metadata contains the expected dump channel
    with open("obj/gnatcov-instr.json") as f:
        params = json.load(f)
    thistest.fail_if_not_equal(
        "actual dump trigger",
        "atexit",
        params["auto-dump-trigger"],
    )
    thistest.fail_if_not_equal(
        "actual dump trigger",
        False,
        params["manual-dump-trigger"],
    )
    thistest.fail_if_not_equal(
        "actual dump channel",
        expected_dump_channel,
        params["dump-channel"],
    )

thistest.result()
