import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xcov,
    xrun,
)


Wdir("tmp_")

# Build and run a program with an "assert" function whose body is covered by an
# exemption region.
gpr = gprfor(srcdirs=[".."], mains=["p.adb"])
gprbuild(gpr)
xrun(exepath_to("p"))

# Exercise branch coverage different ways ...
#
# Without any sco/ali designation, the exemption region is expected not to be
# honored. With --scos or --alis designating the proper ali, the exemption
# region is expected to exempt something:
base_coverage = [
    "coverage",
    "--level=branch",
    "--annotate=xcov",
    tracename_for("p"),
]


def check(ali_spec):
    xcov(base_coverage + ali_spec)
    this_report = contents_of("p.adb.xcov")

    xnote_never_true = r"\*" if ali_spec else "!"
    xnote_uncovered = r"\*" if ali_spec else "-"
    hint = " ".join(ali_spec) if ali_spec else "no ali"
    thistest.fail_if(
        not re.search(
            pattern="%s:.*# expect-never-true" % xnote_never_true,
            string=this_report,
        ),
        "%s, unmatched expected-never-true" % hint,
    )
    thistest.fail_if(
        not re.search(
            pattern="%s:.*# expect-uncovered" % xnote_uncovered,
            string=this_report,
        ),
        "%s, unmatched expected-uncovered" % hint,
    )


check(ali_spec=[])
check(ali_spec=["--scos=obj/p.ali"])
check(ali_spec=["--alis=obj/p.ali"])
check(ali_spec=["-P", "gen.gpr"])
thistest.result()
