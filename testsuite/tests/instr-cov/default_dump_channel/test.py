"""
Test that programs instrumented with the default dump channel (no
--dump-channel argument) behave as expected given the --dump-filename-*
arguments that are passed (or omitted) to "gnatcov instrument".

gnatcov used to ignore --dump-trigger and --dump-filename-* options when
--dump-channel was omitted.
"""

from __future__ import annotations

from dataclasses import dataclass, field
import glob
import os

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import (
    exename_for,
    exepath_to,
    gprbuild,
    gprfor,
    run_cov_program,
    xcov,
)


@dataclass
class Test:
    label: str
    """Simple string to identify the test."""

    srctrace_pattern: str
    """Glob pattern for the expected source trace file."""

    options: list[str] = field(default_factory=list)
    """List of additional options for "gnatcov instrument"."""

    env: dict[str, str] = field(default_factory=dict)
    """Additional environment variables for the executed program."""


# Default prefix for the created source trace ("main.exe" on Windows)
default_prefix = exename_for("main")

for t in [
    Test("none", f"{default_prefix}-*-*-*-0.srctrace"),
    Test("simple", f"{default_prefix}.srctrace", ["--dump-filename-simple"]),
    Test("prefix", "bar-*-*-*-0.srctrace", ["--dump-filename-prefix=bar"]),
    Test(
        "env-var",
        "mytrace.srctrace",
        ["--dump-filename-env-var=MY_TRACE_FILE"],
        {"MY_TRACE_FILE": "mytrace.srctrace"},
    ),
]:
    tmp = Wdir(f"tmp_{t.label}")

    prj = gprfor(mains=["main.adb"], srcdirs=[".."])
    xcov(
        ["instrument", "-P", prj, "-cstmt", "--dump-trigger=atexit"]
        + t.options
    )
    gprbuild(prj, trace_mode="src")

    env = dict(os.environ)
    env.update(t.env)
    run_cov_program(exepath_to("main"), out="run.log", env=env)

    thistest.fail_if(
        len(glob.glob(t.srctrace_pattern)) != 1,
        f"{t.label}: could not find a file matching {t.srctrace_pattern}",
    )

    tmp.to_homedir()

thistest.result()
