import os.path
import re

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import Csw, GPRswitches, gprcov_for
from SUITE.tutils import gprfor, xcov


pgm = "test_lt0"


def run(subdir, extra_args, covlevel=None):
    """
    Build and run the single test program, which volontarily performs stmt and
    decision coverage violations.
    """
    dirname = f"tmp_{subdir}"
    wd = Wdir(dirname)

    gpr = gprfor(
        mains=[pgm + ".adb"],
        srcdirs=["../src"],
        extra=gprcov_for(
            switches=[
                Csw("*", ["--level=stmt"]),
                Csw("coverage", ["--annotate=report"]),
            ]
        ),
    )

    xcov_args = build_and_run(
        gprsw=GPRswitches(root_project=gpr),
        covlevel=covlevel,
        mains=[pgm],
        extra_coverage_args=[] if covlevel is None else ["--level", covlevel],
    )
    xcov(xcov_args + extra_args)

    wd.to_homedir()
    return dirname


def check_report(label, filename, pattern, check_present=True):
    report = contents_of(filename)
    matched = re.search(pattern, report)
    thistest.fail_if(not matched if check_present else matched, label)


# Check that we get results corresponding to the project file
# defaults if we don't tell anything otherwise.

subdir = run("default", ["-o", "def.rep"])
def_rep = os.path.join(subdir, "def.rep")

# Check that we have the output report, that it does contain at least a stmt
# coverage violation note, and that it doesn't contain any DC related note.
check_report(
    "missing expected stmt coverage failure indication",
    def_rep,
    "statement not executed",
)
check_report(
    "unexpected decision coverage failure indication",
    def_rep,
    "decision outcome .* never exercised",
    check_present=False,
)

# Override --level up to DC. Check that we now find the DC violations we
# expect.

subdir = run("lev", ["-o", "lev.rep"], covlevel="stmt+decision")
lev_rep = os.path.join(subdir, "lev.rep")
check_report(
    "missing expected decision coverage failure indication",
    lev_rep,
    "decision outcome .* never exercised",
)

# Override --annotate only. Expect full coverage on the "if"
# statement.

subdir = run("ann", ["--annotate=xcov"])
check_report(
    "missing expected full coverage indication",
    os.path.join(subdir, "obj", "values.adb.xcov"),
    r"\+:.*if",
)

# Override --annotate and --level. Expect partial coverage on the "if"
# statement.

subdir = run("lev-ann", ["--annotate=xcov"], covlevel="stmt+decision")
check_report(
    "missing expected partial decision coverage indication",
    os.path.join(subdir, "obj", "values.adb.xcov"),
    "!:.*if",
)

thistest.result()
