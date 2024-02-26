"""
Check that gnatcov processes SID files and consolidation checkpoints as
completely separate concepts.

In particular: check that it complains when a consolidation checkpoint is
provided where an SID file is expected and conversely, and check that
information in SID files does not "leak" to consolidation checkpoints.
"""

import re

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of, indent, lines_of
from SUITE.tutils import gprfor, srctracename_for, xcov


tmp = Wdir("tmp_")


def main(i):
    return "main{}".format(i)


def sid(i):
    return "i{}.sid".format(i)


def ckpt(i):
    return "c{}.ckpt".format(i)


def trace(i):
    return srctracename_for("main{}".format(i))


def check_error(argv, log_name, expected_error):
    p = xcov(argv, out=log_name, register_failure=False)
    actual_error = contents_of(log_name).strip()

    expected_re = re.compile(
        # Regexp to accommodate output differences between the various
        # supported platforms.
        "[^\n]*gnatcov[^\n]*: {}".format(re.escape(expected_error))
    )

    thistest.fail_if(p.status == 0, "gnatcov was expected to fail, it did not")
    thistest.fail_if(
        not expected_re.match(actual_error),
        'Unexpected output for "gnatcov coverage" ({}). Expected:\n'
        "{}\n"
        "but got:\n"
        "{}".format(
            log_name, indent(repr(expected_re.pattern)), indent(actual_error)
        ),
    )


gpr = gprfor(["main1.adb", "main2.adb"], srcdirs="..")

# Prepare material for the checks: instrument both main1 and main2, produce
# traces and create consolidation checkpoints for them.
for i in (1, 2):
    sid_file = "obj/main{}.sid".format(i)
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=gpr, units=[main(i)]),
        covlevel="stmt+mcdc",
        mains=[main(i)],
        extra_coverage_args=["--save-checkpoint", ckpt(i), "--sid", sid_file],
        trace_mode="src",
    )
    cp(sid_file, sid(i))


# Check that gnatcov complains when passing a consolidation checkpoint as an
# SID file.
check_error(
    argv=[
        "coverage",
        "--annotate=xcov",
        "--level=stmt+mcdc",
        "--sid",
        ckpt(1),
        trace(1),
    ],
    log_name="ckpt_as_sid.log",
    expected_error=(
        "invalid Source Instrumentation Data (SID) file "
        "{}, name of file should have .sid extension".format(ckpt(1))
    ),
)

# Check that gnatcov complains when passing an SID file as a consolidation
# checkpoint.
check_error(
    argv=[
        "coverage",
        "--level=stmt+mcdc",
        "--checkpoint",
        sid(1),
        "--checkpoint",
        sid(2),
        "--save-checkpoint",
        "consolidated.ckpt",
    ],
    log_name="sid_as_ckpt.log",
    expected_error="{} is a Source Instrumentation Data (SID) while a"
    " checkpoint was expected".format(sid(1)),
)


# Finally, check that information from SID files does not leak into
# checkpoints.  This means that one needs to provide an SID file in order to
# decode the corresponding source traces.
#
# Here, even though c1.ckpt was produced while i1.sid was loaded, one must
# provide i1.sid in order to decode main1.srctrace: loading c1.ckpt is not
# enough.
xcov(
    [
        "coverage",
        "-v",
        "--annotate=xcov",
        "--level=stmt+mcdc",
        "--checkpoint",
        ckpt(1),
        trace(1),
    ],
    out="leak.log",
)
thistest.fail_if(
    "[GNATCOV.MISC] discarding source trace entry for unknown instrumented"
    " unit: body of main1" not in lines_of("leak.log")
)

thistest.result()
