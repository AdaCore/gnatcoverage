"""
Check the SARIF coverage report emitted for a C project containing statement,
decision, mcdc violations some of which are exempted.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

lib = gprfor(
    prjid="lib",
    srcdirs="../src-lib",
    mains=None,
    objdir="obj-lib",
    langs=["C"],
)

foo = gprfor(
    prjid="foo",
    srcdirs="../src-root/src-foo",
    mains=["foo.c"],
    objdir="obj-foo",
    deps=["bar", "lib"],
    langs=["C"],
)

bar = gprfor(
    prjid="bar",
    srcdirs="../src-root/src-foo/src-bar",
    mains=["bar.c"],
    objdir="obj-bar",
    langs=["C"],
)

root = gprfor(
    prjid="gen",
    srcdirs="../src-root",
    mains=["main.c"],
    objdir="obj",
    deps=["lib", "foo"],
    langs=["C"],
)

build_run_and_coverage(
    gprsw=GPRswitches(root),
    covlevel="stmt+mcdc+atcc",
    mains=["main"],
    trace_mode="src",
    extra_coverage_args=["--annotate=sarif"],
)

thistest.fail_if_no_match(
    "SARIF report",
    contents_of("../ref.sarif"),
    contents_of("obj/coverage.sarif"),
)

thistest.result()
