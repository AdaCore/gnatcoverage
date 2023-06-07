"""
Check that the --dump-filename-env-var, --dump-filename-prefix and
--dump-filename-simple options work as expected.
"""

from dataclasses import dataclass
import glob
import os
from typing import Dict, List

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


@dataclass
class Testcase:
    test_name: str
    """Name for this testcase."""

    extra_args: List[str]
    """Extra arguments to pass to "gnatcov instrument"."""

    extra_env_vars: Dict[str, str]
    """Extra environment variables to pass to the instrumented program."""

    expected_trace: str
    """Glob pattern whose expansion must return exactly one source trace."""


trace_prefix = "main"
if thistest.env.target.os.name == "windows":
    trace_prefix += ".exe"

for tc in [
    # Kind of sanity check: only use defaults, like all other testcases
    Testcase("regular", [], {}, f"{trace_prefix}-*.srctrace"),

    # Check that with default instrumentation options and
    # GNATCOV_TRACE_FILE set to a filename, the trace file is created using
    # that filename.
    Testcase("env_filename",
             [],
             {"GNATCOV_TRACE_FILE": "foo.srctrace"},
             "foo.srctrace"),

    # Check that with default instrumentation options and
    # GNATCOV_TRACE_FILE set to a directory name, the trace file is created
    # with a standard name under that directory.
    Testcase("env_dir",
             [],
             {"GNATCOV_TRACE_FILE": "traces-dir/"},
             f"traces-dir/{trace_prefix}-*.srctrace"),

    # Check that with --dump-filename-env-var, the name of the environment
    # variable changes.
    Testcase("foo_env_1",
             ["--dump-filename-env-var=FOO"],
             {"GNATCOV_TRACE_FILE": "foo.srctrace"},
             f"{trace_prefix}-*.srctrace"),
    Testcase("foo_env_2",
             ["--dump-filename-env-var=FOO"],
             {"FOO": "foo.srctrace"},
             "foo.srctrace"),

    # Check that with --dump-file-name-prefix, the given prefix replaces the
    # program basename.
    Testcase("prefix", ["--dump-filename-prefix=bar"], {}, "bar-*.srctrace"),

    # Check that with --dump-file-name-simple, the produced trace has a
    # deterministic name.
    Testcase("simple",
             ["--dump-filename-simple"],
             {},
             f"{trace_prefix}.srctrace"),
]:
    thistest.log(f"==== {tc.test_name} ====")

    wd = Wdir(f"tmp_{tc.test_name}")
    os.mkdir("traces-dir")
    try:
        env = dict(os.environ)
        env.update(tc.extra_env_vars)

        xcov_args = build_and_run(
            gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."],
                              mains=["main.adb"])),
            covlevel="stmt",
            mains=["main"],
            extra_instr_args=tc.extra_args,
            extra_coverage_args=["-axcov", "--output-dir=xcov"],
            trace_mode="src",
            program_env=env,

            # The very goal of this testcase is to produce trace files with
            # non-default names. build_and_run cannot deal with them, so skip
            # checks here: the computing below are enough to check that the
            # expected source trace file was produced.
            register_failure=False,
        )

        # Check that we have the expected trace:
        traces = glob.glob(tc.expected_trace)
        if not traces:
            thistest.fail_if(
                True,
                f"No source trace matched {tc.expected_trace}",
            )
            continue
        if len(traces) > 1:
            thistest.fail_if(
                True,
                f"Too many source trace matched {tc.expected_trace}:\n"
                + "\n".join(f"* {tf}" for tf in traces)
            )
            continue

        # Sanity check that expected trace
        xcov(xcov_args + traces, out="coverage.log")
        check_xcov_reports("xcov/*.xcov", {
            "xcov/main.adb.xcov": {"+": {5}},
        })
    finally:
        wd.to_homedir()

thistest.result()
