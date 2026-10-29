from __future__ import annotations

from dataclasses import dataclass
import glob

from SUITE.context import thistest


@dataclass
class Testcase:
    test_name: str
    """Name for this testcase."""

    extra_args: list[str]
    """
    Extra arguments to pass to "gnatcov instrument" or "gnatcov
    setup-instrumentation".
    """

    extra_env_vars: dict[str, str]
    """Extra environment variables to pass to the instrumented program."""

    expected_trace: str
    """Glob pattern whose expansion must return exactly one source trace."""

    def find_traces(self) -> list[str]:
        """
        Check that we have the expected traces and return them.

        If trace files are unexpected, return the empty list.
        """
        traces = glob.glob(self.expected_trace)
        if not traces:
            thistest.fail_if(
                True,
                f"No source trace matched {self.expected_trace}",
            )
            return []
        if len(traces) > 1:
            thistest.fail_if(
                True,
                f"Too many source trace matched {self.expected_trace}:\n"
                + "\n".join(f"* {tf}" for tf in traces),
            )
            return []

        return traces


trace_prefix = "main"
if thistest.env.target.os.name == "windows":
    trace_prefix += ".exe"


testcases = [
    # Kind of sanity check: only use defaults, like all other testcases
    Testcase("regular", [], {}, f"{trace_prefix}-*.srctrace"),
    # Check that with default instrumentation options and
    # GNATCOV_TRACE_FILE set to a filename, the trace file is created using
    # that filename.
    Testcase(
        "env_filename",
        [],
        {"GNATCOV_TRACE_FILE": "foo.srctrace"},
        "foo.srctrace",
    ),
    # Check that with default instrumentation options and
    # GNATCOV_TRACE_FILE set to a directory name, the trace file is created
    # with a standard name under that directory.
    Testcase(
        "env_dir",
        [],
        {"GNATCOV_TRACE_FILE": "traces-dir/"},
        f"traces-dir/{trace_prefix}-*.srctrace",
    ),
    # Check that with --dump-filename-env-var, the name of the environment
    # variable changes.
    Testcase(
        "foo_env_1",
        ["--dump-filename-env-var=FOO"],
        {"GNATCOV_TRACE_FILE": "foo.srctrace"},
        f"{trace_prefix}-*.srctrace",
    ),
    Testcase(
        "foo_env_2",
        ["--dump-filename-env-var=FOO"],
        {"FOO": "foo.srctrace"},
        "foo.srctrace",
    ),
    # Check that with --dump-filename-prefix, the given prefix replaces the
    # program basename.
    Testcase("prefix", ["--dump-filename-prefix=bar"], {}, "bar-*.srctrace"),
    # Check that with --dump-filename-tag, the given tag replaces the
    # autocomputed tag.
    Testcase(
        "tag",
        ["--dump-filename-tag=bar"],
        {},
        f"{trace_prefix}-bar-*.srctrace",
    ),
    # Check that with --dump-filename-simple, the produced trace has a
    # deterministic name.
    Testcase(
        "simple", ["--dump-filename-simple"], {}, f"{trace_prefix}.srctrace"
    ),
]
