"""
Test that `pragma Ada_83` nodes are correctly removed during instrumentation.
"""

from SCOV.minicheck import build_and_run, thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor

_tmp = Wdir("tmp_")

p = gprfor(mains=["main.adb"], srcdirs="../src")

build_and_run(
    GPRswitches(root_project=p, units=["foo"]),
    mains=["main"],
    covlevel="stmt",
    extra_coverage_args=[],
)

for file in ["main.adb", "foo.ads", "foo.adb"]:
    thistest.fail_if_match(
        "Shouldn't find Ada_83 in the instrumented source file",
        ".*pragma Ada_83.*",
        contents_of(f"obj/gen-gnatcov-instr/{file}"),
    )


thistest.result()
