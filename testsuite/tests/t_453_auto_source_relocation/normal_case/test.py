"""
Ensure that the automatic source rebasing works, and fails when
file unicity is not guaranteed.
"""

import re
from shutil import copytree, rmtree

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov

wd = Wdir("tmp_")

copytree("../src", "src_copy")

gpr_orig = gprfor(
    ["main.adb"],
    prjid="orig",
    srcdirs=["../src"],
    objdir="obj",
    langs=["ada", "c"],
)
gpr_copy = gprfor(
    ["main.adb"],
    prjid="copy",
    srcdirs=["src_copy"],
    objdir="obj",
    langs=["ada", "c"],
)

args: list[str] = build_and_run(
    gprsw=GPRswitches(root_project=gpr_copy),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
)
args_orig = [
    arg if not arg.endswith(".gpr") else f"-P{gpr_orig}" for arg in args
] + [
    "-a",
    "xcov",
]

# Remove the sources that were used to instrument and build the executable,
# So gnatcov has to find them elsewhere.
rmtree("src_copy")

xcov(args_orig)

SRC_FILES = {"foo.c", "pkg.adb", "main.adb"}
if thistest.options.trace_mode != "bin":
    SRC_FILES.add("pkg.ads")


# Make sure that when disabling auto-relocation, gnatcov fails to find the
# source files
xcov(
    args_orig + ["--no-auto-source-relocation"],
    tolerate_messages=f"can't open .*({'|'.join(SRC_FILES)})",
    out="coverage_output.txt",
)

output = contents_of("coverage_output.txt")

for src in SRC_FILES:
    expected = re.compile(f".*warning: can't open .*{src}.*", flags=re.DOTALL)
    thistest.fail_if_no_match(
        f"Should emit a warning message for failing to find {src}",
        expected,
        output,
    )

thistest.result()
