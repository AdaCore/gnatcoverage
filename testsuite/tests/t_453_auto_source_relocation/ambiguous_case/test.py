"""
Ensure that ambiguous files names are not resolved.
"""

import re
from shutil import copytree, rmtree

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov

tmp = Wdir("tmp_")

libfoo_gpr = gprfor(
    prjid="libfoo",
    mains=[],
    srcdirs=["../libfoo"],
    objdir="obj_lib",
    extra="""
        for Library_Kind use "static";
        for Library_Name use "foo";
        for Library_Dir use "lib";
    """,
)

app_gpr = gprfor(
    prjid="app",
    mains=["main.adb"],
    srcdirs=["../app"],
    deps=["libfoo"],
)

copytree("../libfoo", "libfoo_copy")
libfoo_gpr = gprfor(
    prjid="libfoo_cp",
    mains=[],
    srcdirs=["libfoo_copy"],
    objdir="obj_lib",
    extra="""
        for Library_Kind use "static";
        for Library_Name use "foo";
        for Library_Dir use "lib";
    """,
)
copytree("../app", "app_copy")
app_copy_gpr = gprfor(
    prjid="app_cp",
    mains=["main.adb"],
    srcdirs=["app_copy"],
    deps=["libfoo_cp"],
)

args = build_and_run(
    GPRswitches(
        root_project=app_copy_gpr,
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
)
args = [f"-P{app_gpr}" if arg.startswith("-P") else arg for arg in args] + [
    "-a",
    "xcov",
]

# Remove the file made
rmtree("libfoo_copy")
rmtree("app_copy")

# Run gnatcov coverage and ensure that the program succeeds with warnings.
xcov(args, out="coverage.out", tolerate_messages=r"can't open .*foo\.c")

output = contents_of("coverage.out")

# Check the presence of warnings
pattern = re.compile(r"warning: can't open .*foo\.c")
matches = re.findall(pattern, output)
thistest.fail_if(
    len(matches) != 2,
    f"Expected 2 lines 'warning: can't open .../foo.c', got {len(matches)}",
)

thistest.result()
