"""
Test that gnatcov properly reject non-library aggregate projects
"""

from e3.fs import cp

from SCOV.minicheck import xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import contents_of


def check_cmd(cmd, extra_args=None, trace_mode=None):
    """
    Check that the gnatcov command cmd properly rejects the aggregate project
    aggr.gpr, emitting the EXPECTED_ERR_MSG error message.

    The check is only performed the the current trace mode matches trace_mode,
    if not None.

    extra_args are concatenated to the arguments passed to xcov.
    """
    if trace_mode and thistest.options.trace_mode != trace_mode:
        return

    log = f"{cmd}.log"
    p_cmd = xcov(
        [cmd, "-Paggr.gpr", "-q", "-cstmt"]
        + (extra_args if extra_args else []),
        register_failure=False,
        out=log,
    )
    thistest.fail_if(
        p_cmd.status == 0,
        comment=f"'gnatcov {cmd}' did not exit with an error status",
    )
    thistest.fail_if_no_match(
        f"unexpected or empty 'gnatcov {cmd}' error message",
        regexp=r".*gnatcov(.exe)?:"
        r" non-library aggregate projects are not supported",
        actual=contents_of(log).strip(),
    )


tmp = Wdir("tmp_")

# copy the projects in the test directory to avoid having lingering artifacts
cp("../*.gpr", ".")

trace_suffix = "srctrace" if thistest.options.trace_mode == "src" else "trace"

# Test all the gnatcov subcommands, when relevant to the trace mode, properly
# reject aggregate projects.
tests = [
    ("instrument", None, "src"),
    ("run", ["dummy_exe"], "bin"),
    ("coverage", [f"dummy.{trace_suffix}"], None),
]

for cmd, extra, mode in tests:
    check_cmd(cmd, extra, mode)

thistest.result()
