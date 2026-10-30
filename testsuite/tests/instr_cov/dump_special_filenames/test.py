"""
Check that "gnatcov instrument" produces legal and functional sources for mains
with --dump-channel=bin-file when the various dump parameters are passed
special characters.
"""

import glob
import os.path

from SCOV.instr import xcov_instrument
from SCOV.minicheck import check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import exepath_to, gprbuild, gprfor, run_cov_program, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

special_chars = "\"\n\\??'"

prefix = f"pfx_{special_chars}_val"
env_var = f"ENV_{special_chars}_VAR"
tag = f"TAG_{special_chars}_VAL"

# Create, instrument and build the test project
gpr = gprfor(
    srcdirs=[".."],
    mains=["ada_main.adb", "c_main.c"],
    compiler_extra="""
        --  Force the use of C99 so that trigraphs are interpreted
        for Default_Switches ("C") use ("-std=c99");
    """,
)
xcov_instrument(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt",
    extra_args=[
        f"--dump-filename-prefix={prefix}",
        f"--dump-filename-env-var={env_var}",
        f"--dump-filename-tag={tag}",
    ],
)
gprbuild(gpr)

# Run the mains, collect expected traces and check the coverage report produced
# from them.
#
# We do this twice:
#
# 1. Once with the default environment, to exercise --dump-filename-prefix and
#    --dump-filename-tag
#
# 2. Once with the environment variable set to exercise
#    --dump-filename-env-var.
mains = ["ada_main", "c_main"]
for label, env_fn, trace_pattern_fn in [
    (
        "default",
        lambda _: {},
        lambda _: f"{prefix}-{tag}-*.srctrace",
    ),
    (
        "env",
        lambda m: {env_var: os.path.abspath(f"manual_{m}.srctrace")},
        lambda m: f"manual_{m}.srctrace",
    ),
]:
    thistest.log(f"== {label} ==")
    traces = []
    xcov_dir = f"xcov_{label}"
    for m in mains:
        run_cov_program(exepath_to(m), env=env_fn(m))
        trace_pattern = trace_pattern_fn(m)
        m_traces = glob.glob(trace_pattern)
        thistest.log(f"Adding traces matching {trace_pattern!r}: {m_traces!r}")
        traces += m_traces

    xcov(
        [
            "coverage",
            "-P",
            gpr,
            "--level=stmt",
            "--annotate=xcov",
            "--output-dir",
            xcov_dir,
            *traces,
        ]
    )
    check_xcov_reports(
        xcov_dir,
        {
            "ada_main.adb.xcov": {"+": {3}},
            "c_main.c.xcov": {"+": {4}},
        },
    )

thistest.result()
