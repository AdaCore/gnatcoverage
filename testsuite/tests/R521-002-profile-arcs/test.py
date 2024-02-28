import re

from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import (
    exepath_to,
    gprfor,
    gprbuild,
    thistest,
    tracename_for,
    xcov,
    xrun,
)


wd = Wdir("tmp_")
main = exepath_to("main")
main_trace = tracename_for("main")

gpr = gprfor(["main.adb"], srcdirs="..", langs=("Ada", "C"))
gprbuild(
    gpr, extracargs="-fprofile-arcs", largs="-fprofile-arcs -ftest-coverage"
)

xrun(main)
xcov(
    ["coverage", "-P", gpr, "-cstmt+decision", "-axml", main_trace],
    out="coverage.log",
)

warning_template = (
    r"warning: .*{}: unit compiled with instrumentation" r" \(-fprofile-arcs\)"
)

for unit in ("main.ali", "util.c.gli"):
    thistest.fail_if(
        not any(
            re.match(warning_template.format(re.escape(unit)), line)
            for line in lines_of("coverage.log")
        ),
        "Warning about -fprofile-arcs for {} not found in output of"
        ' "gnatcov coverage"'.format(unit),
    )

thistest.result()
