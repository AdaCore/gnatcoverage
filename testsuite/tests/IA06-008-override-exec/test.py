"""Check that --exec works properly."""

import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    unixpath_to,
    xcov,
    xrun,
)


wd = Wdir("tmp_")

gprbuild(gprfor(srcdirs=["../src"], mains=["noop.adb"]))
xrun(unixpath_to("noop"))

# On Windows, we cannot override an existing file during a renaming.
if os.path.exists("nop"):
    os.unlink("nop")
os.rename(exepath_to("noop"), "nop")

p = xcov(
    ["coverage", "--level=branch", "--annotate=asm", tracename_for("noop")],
    out="not_found.out",
    register_failure=False,
)
thistest.fail_if(
    p.status == 0
    or not match(
        "Cannot open ELF file %s for trace file" % unixpath_to("noop"),
        "not_found.out",
    ),
    "no error if exec not found",
)

# We are not interested in the exact coverage status of the main symbol, so
# just look if we have some coverage indication for it as a sanity check that
# the coverage analysis request did something. Beware that "main" might be
# named after the main subprogram on VxWorks systems.
xcov(
    [
        "coverage",
        "--level=branch",
        "--annotate=asm",
        "--exec=nop",
        tracename_for("noop"),
    ],
    out="noop.stdout",
)
thistest.fail_if(
    not match("main|noop [+!]:", "noop.stdout"),
    "--exec overrides trace file info",
)

thistest.result()
