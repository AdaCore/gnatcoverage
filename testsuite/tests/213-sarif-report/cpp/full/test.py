"""
Check the SARIF coverage report emitted for a C++ project containing statement,
decision, mcdc violations, some of which are exempted.
"""

from SCOV.minicheck import build_run_and_coverage
from SCOV.sarif import check_sarif_report
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

lib = gprfor(
    prjid="lib",
    srcdirs="../src-lib",
    mains=None,
    objdir="obj-lib",
    langs=["C++"],
)

foo = gprfor(
    prjid="foo",
    srcdirs="../src-root/src-foo",
    mains=["foo.cpp"],
    objdir="obj-foo",
    deps=["bar", "lib"],
    langs=["C++"],
)

bar = gprfor(
    prjid="bar",
    srcdirs="../src-root/src-foo/src-bar",
    mains=["bar.cpp"],
    objdir="obj-bar",
    langs=["C++"],
)

root = gprfor(
    prjid="gen",
    srcdirs="../src-root",
    mains=["main.cpp"],
    objdir="obj",
    deps=["lib", "foo"],
    langs=["C++"],
)

build_run_and_coverage(
    gprsw=GPRswitches(root),
    covlevel="stmt+mcdc+atcc",
    mains=["main"],
    trace_mode="src",
    extra_coverage_args=["--annotate=sarif"],
)

check_sarif_report("../ref.sarif", "obj/coverage.sarif")

thistest.result()
