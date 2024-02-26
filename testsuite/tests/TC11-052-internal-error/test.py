"""
Check that gnatcov reports internal errors as expected.
"""

import re
import os

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


def check(args, trigger, info):
    # Run gnatcov with the given arguments and trigger
    out = f"{trigger}.log"
    env = dict(os.environ)
    env["GNATCOV_INTERNAL_ERROR_TRIGGER"] = trigger
    p = xcov(args + ["-q"], out=out, env=env, register_failure=False)
    thistest.fail_if(
        p.status == 0, "gnatcov returned success exit code, error expected"
    )

    trigger_msg = trigger.upper().replace("-", "_")
    expected_output = re.compile(
        "\n".join(
            [
                ".*",
                "== gnatcov bug detected ==",
                "",
                "gnatcov just encountered an internal error:",
                f"raised CONSTRAINT_ERROR : {trigger_msg}",
                # Depending on how gnatcov is built, exception info may contain
                # stack traces.
                "((Load address: [x0-9a-f]+\n)?Call stack traceback"
                " locations:",
                "[x0-9a-f ]+",
                ")?",
                "",
                "(?P<info>.*)",
                "",
                "This is gnatcov version .*",
            ]
        ),
        re.MULTILINE,
    )

    # The difference between expected and actual output generally is
    # specifically in the "info" part. To make error messages easier to read,
    # first match the global structure of the output. If that succeeds, compare
    # the "info" part separately.
    output = contents_of(out)
    thistest.fail_if_no_match(
        what=f"[{trigger}] gnatcov output",
        regexp=expected_output,
        actual=output,
    )

    m = expected_output.match(output)
    if m:
        thistest.fail_if_no_match(
            what=f"[{trigger}] context info in gnatcov output",
            regexp=re.compile(info, re.MULTILINE),
            actual=m.group("info"),
        )


tmp = Wdir("tmp_")
prj = gprfor(srcdirs=[".."], mains=["main.adb"])
os.mkdir("obj")

check(
    args=["coverage"],
    trigger="arguments-loading",
    info=r"No coverage processing context information available\.",
)

check(
    args=["instrument", "-P", prj, "--level=stmt"],
    trigger="ada-instrument-start-file",
    info=r"Instrumenting [^\n]*main\.adb",
)

check(
    args=["instrument", "-P", prj, "--level=stmt"],
    trigger="ada-instrument-null-proc",
    info=r"Instrumenting NullSubpDecl at [^\n]*main\.adb:7:4-7:33",
)

check(
    args=["instrument", "-P", prj, "--level=stmt"],
    trigger="ada-instrument-insert-stmt-witness",
    info=r"Instrumenting ConcreteTypeDecl at [^\n]*main\.adb:6:4-6:26",
)

# Instrument the example project, run its main to produce a source trace and
# then trigger an internal error while loading the SID file.
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
    trace_mode="src",
)
check(
    args=xcov_args,
    trigger="load-checkpoint",
    info=r"Loading [^\n]*main\.sid",
)

thistest.result()
