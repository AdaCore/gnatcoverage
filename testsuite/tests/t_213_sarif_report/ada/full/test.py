"""
Check the SARIF coverage report emitted for an Ada project containing
statement, decision, mcdc, atc, and atcc violations, some of which are
exempted.
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
    extra="""
        for Library_Name use Project'Name;
        for Library_Dir use "lib-" & Project'Name;
    """,
)

foo = gprfor(
    prjid="foo",
    srcdirs="../src-root/src-foo",
    mains=None,
    objdir="obj-foo",
    deps=["bar", "lib"],
    extra="""
        for Library_Name use Project'Name;
        for Library_Dir use "lib-" & Project'Name;
    """,
)

bar = gprfor(
    prjid="bar",
    srcdirs="../src-root/src-foo/src-bar",
    mains=None,
    objdir="obj-bar",
    extra="""
        for Library_Name use Project'Name;
        for Library_Dir use "lib-" & Project'Name;
    """,
)

root = gprfor(
    prjid="gen",
    srcdirs="../src-root",
    mains=["main.adb"],
    objdir="obj",
    deps=["lib", "foo"],
)

instr_msg = (
    r"\*\*\* a.ads:8:3: warning: gnatcov limitation: cannot instrument an"
    " expression function which is a primitive of its return type, when this"
    r" type is a tagged type\. Consider turning it into a regular function"
    r" body\."
)

build_run_and_coverage(
    gprsw=GPRswitches(root),
    covlevel="stmt+mcdc+atcc",
    mains=["main"],
    trace_mode="src",
    extra_coverage_args=["--annotate=sarif"],
    tolerate_instrument_messages=instr_msg,
)

check_sarif_report("../ref.sarif", "obj/coverage.sarif")

thistest.result()
