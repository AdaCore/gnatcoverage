"""
Check that gnatcov coverage correctly aggregates coverage data of a file that
has various version according to the preprocessing configuration.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

prj = gprfor(srcdirs=[".."], mains=["main.c"])

# Compute coverage data with the A configuration
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--c-opts=-DA"],
    extra_coverage_args=["--cancel-annotate", "--save-checkpoint", "a.ckpt"],
    trace_mode="src",
)

# Compute coverage data for the B configuration
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--c-opts=-DB"],
    extra_coverage_args=["--cancel-annotate", "--save-checkpoint", "b.ckpt"],
    trace_mode="src",
)

# Aggregate the coverage data to produce an xcov report
xcov(
    ["coverage", "-P", prj, "--level=stmt", "-axcov", "-Ca.ckpt", "-Cb.ckpt"],
    tolerate_messages="warning: inconsistent preprocessing information.*",
)

check_xcov_reports(
    "obj",
    {"main.c.xcov": {"+": {5, 8}}},
    discard_empty=False,
)

thistest.result()
