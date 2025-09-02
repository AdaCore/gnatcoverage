"""
Check that the "gnatcov setup" command works as expected.

Note that all tests use a non-standard project name for the installed
instrumentation runtime project, as a way to avoid checks working just because
they actually use the runtime project already present in the testsuite
environment.

Also note that, due to the testsuite infrastructure, we cannot test the
"default prefix", i.e. letting gprinstall put the instrumentation runtime
project in the toolchain prefix, as the testsuite is not allowed to modify it.
"""

from dataclasses import dataclass
import os.path
from typing import List, Dict, Set

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprbuild, gprfor, xcov
from SUITE.gprutils import GPRswitches


# Project file for an extended runtime
rt_ext_file = os.path.join(os.path.abspath("ext_rt"), "ext_rt.gpr")


tmp = Wdir("tmp_")

# Name of the installed instrumentation runtime project
rt_prj = "rt_prj"

# Installation directory for the instrumentation runtime project and directory
# for the project path.
rt_install_dir = os.path.abspath("install")
rt_path_dir = os.path.join(rt_install_dir, "share", "gpr")

# Make this install available to GPR tools
env.add_search_path("GPR_PROJECT_PATH", rt_path_dir)

ExpectedCov = Dict[str, Dict[str, Set[int]]]


@dataclass
class PrjConfig:
    """
    Project file, executable name and expected coverage report.
    """

    project_file: str
    objdir: str
    expected_cov: ExpectedCov

    @classmethod
    def create(
        cls, tag: str, langs: List[str], main: str, expected_cov: ExpectedCov
    ):
        objdir = f"obj-{tag}"
        return cls(
            project_file=gprfor(
                prjid=f"{tag}_prj",
                langs=langs,
                mains=[main],
                srcdirs=[f"../src-{tag}"],
                objdir=objdir,
            ),
            objdir=objdir,
            expected_cov=expected_cov,
        )

    @property
    def gprsw(self):
        return GPRswitches(root_project=self.project_file)


# Create simple projects to host our experiments
ada_prj = PrjConfig.create(
    tag="ada",
    langs=["Ada"],
    main="foo.adb",
    expected_cov={"foo.adb.xcov": {"+": {5}}},
)
c_prj = PrjConfig.create(
    tag="c",
    langs=["C"],
    main="foo.c",
    expected_cov={"foo.c.xcov": {"+": {6, 7}}},
)
mixed_prj = PrjConfig.create(
    tag="mixed",
    langs=["Ada", "C"],
    main="foo.adb",
    expected_cov={
        "foo.adb.xcov": {"+": {11}},
        "pkg.ads.xcov": {"+": {5, 6}},
        "pkg.adb.xcov": {"+": {7}},
        "bar.c.xcov": {"+": {6}},
    },
)


def xsetup(args, out, register_failure=True, auto_config_args=True):
    """
    "xcov" wrapper to run "gnatcov setup".

    This wrapper just automatically pass --install-name and --prefix
    arguments.
    """
    return xcov(
        [
            "setup",
            f"--install-name={rt_prj}",
            f"--prefix={rt_install_dir}",
            "-v",
        ]
        + args,
        out=out,
        register_failure=register_failure,
        auto_config_args=auto_config_args,
        force_project_args=True,
    )


def check_full(
    label,
    setup_args,
    prj_config,
    runtime_project=rt_prj,
    dump_channel="auto",
    auto_config_args=True,
):
    """
    Run "gnatcov setup" with the arguments (setup_args), then compute code
    coverage and check the report for the given project (prj_config).

    Also check that `gnatcov instrument` successfully loaded the setup
    configuration file.
    """
    thistest.log(f"== {label} ==")
    xsetup(setup_args, f"setup-{label}.txt", auto_config_args=auto_config_args)
    build_run_and_coverage(
        gprsw=prj_config.gprsw,
        covlevel="stmt",
        mains=["foo"],
        gpr_obj_dir=prj_config.objdir,
        extra_coverage_args=["-axcov", f"--output-dir=xcov-{label}"],
        trace_mode="src",
        runtime_project=runtime_project,
        dump_channel=dump_channel,
        extra_instr_args=["-v"],
    )
    thistest.fail_if(
        "Successfully loaded the setup configuration file"
        not in contents_of("instrument.log"),
        "Failed to load the setup config file in `gnatcov instrument`",
    )
    check_xcov_reports(f"xcov-{label}", prj_config.expected_cov)


# "gnatcov setup" (+ relevant target options) is supposed to work out of the
# box and use most of the RTS capabilities.
check_full("basic-mixed", [], mixed_prj)

# Check that --restricted-to-languages works as expected
check_full("basic-ada", ["--restricted-to-languages=Ada,C"], ada_prj)

# Our current testsuite runs use Ada runtimes, which do not provide fwrite and
# other similar routines on embedded targets. This means that we cannot test
# our C-only instrumentation runtime on these targets.
#
# Do not pass the testsuite project configuration file to gnatcov setup as it
# was generated for Ada, C and C++, and thus defines Ada specific attributes
# (such as the runtime directory) which would not normally be present in a C
# only project.
if not env.is_cross:
    check_full(
        "basic-c",
        ["--restricted-to-languages=C"],
        c_prj,
        auto_config_args=False,
    )

# The core runtime is implemented in C, so C must be enabled
thistest.log("== ada-only ==")
log_file = "setup-ada-only.txt"
p = xsetup(["--restricted-to-languages=Ada"], log_file, register_failure=False)
thistest.fail_if(
    p.status == 0, "gnatcov setup succeeded when we expected it to fail"
)
thistest.fail_if_no_match(
    log_file,
    "(.|\n)*The C language must be enabled(.\n)*",
    contents_of(log_file),
)

# ... so also check that misusing it fails, as expected too: the previous tests
# could fail because --restricted-to-languages is actually ignored (and thus
# the installed runtime always support both Ada and C). Here, we check that
# actually have an error when trying to build instrumented Ada code while the
# runtime has support for C only.
#
# Same remark as above concerning the configuration file.
thistest.log("== basic-bad-lang ==")
log_file = "gprbuild-basic-bad-lang.txt"
xsetup(
    ["--restricted-to-languages=C"],
    "setup-basic-bad-lang.txt",
    auto_config_args=False,
)
xcov_instrument(
    gprsw=mixed_prj.gprsw,
    covlevel="stmt",
    runtime_project=rt_prj,
    out="instr-basic-bad-lang.txt",
    # Since "gnatcov setup" excluded Ada from the coverage runtime languages,
    # no runtime was used to compile it, and thus it is expected that we get a
    # discrepancy between "rt_prj" (no runtime) and "mixed_prj" (current
    # testsuite runtime).
    tolerate_messages="Current runtime is",
)
p = gprbuild(
    mixed_prj.project_file,
    gargs=["--src-subdirs=gnatcov-instr", "-q"],
    trace_mode="src",
    runtime_project=rt_prj,
    out=log_file,
    register_failure=False,
)
thistest.fail_if(
    p.status == 0, "gprbuild succeeded when we expected it to fail"
)
thistest.fail_if_no_match(
    log_file,
    '.*foo.adb:.*"gnatcov_rts.ads" not found.*',
    contents_of(log_file),
)

# Check the installation of an alternative runtime project. Our modified
# runtime (designed for base64-stdout only) just prints a "Hello, world", so we
# can check that our modified runtime is correctly used looking at the main
# output.
#
# Accept just the hello world message or more, as the output may contain a
# base64 trace depending on the testsuite mode.
check_full("ext_rt", [rt_ext_file], ada_prj, dump_channel="base64-stdout")
thistest.fail_if(
    "<TAG>" not in contents_of("foo_output.txt"),
    "<TAG> not in the program output",
)

# Now, check that -gargs arguments are passed to gprbuild. To do this, build
# the instrumentation runtime with -gnata. This will make GNAT compile with
# assertions enabled (they are disabled by default), and will thus trigger the
# execution of a Put_Line that is otherwise disabled.
check_full(
    "ext_rt",
    [rt_ext_file, "-gargs", "-cargs:Ada", "-gnata"],
    ada_prj,
    dump_channel="base64-stdout",
)
thistest.fail_if(
    "<PREDICATE>" not in contents_of("foo_output.txt"),
    "<PREDICATE> not in the program output",
)

# Check C89 backward-compatibility, as we want to support instrumentation
# with older toolchains.
check_full(
    "ext_rt",
    [rt_ext_file, "-gargs", "-cargs:C", "-std=c89"],
    ada_prj,
    dump_channel="base64-stdout",
)

# Check that passing a full path to --runtime-project works
check_full(
    "full_path",
    [],
    ada_prj,
    runtime_project=os.path.join(rt_path_dir, rt_prj),
)

thistest.result()
