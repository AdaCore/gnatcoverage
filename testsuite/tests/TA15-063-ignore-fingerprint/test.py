"""
Check that the consolidation of two checkpoints produced with different
--excluded-source-files options on the same set of sources works as expected. A
warning used to be emitted in that case, because of a low level SCO tables
fingerprint mismatch.
"""

import os.path

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


def check_empty(filename):
    content = contents_of(filename)
    thistest.fail_if(
        content,
        f'Output of "gnatcov coverage" not empty ({filename}):\n\n{content}',
    )


tmp = Wdir("tmp_")

p = gprfor(prjid="p", srcdirs=["../src"], mains=["main.adb"])
test1 = os.path.abspath("test1.gpr")
cp("../test1.gpr", test1)

# Build and run p.gpr's main.adb, then compute a partial coverage report
# (c1.ckpt), ignoring no source file.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=p, projects=["p"]),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--save-checkpoint=c1.ckpt"],
    out="log-cov1.txt",
)
check_empty("log-cov1.txt")

# Build and run test1.gpr's main.adb, then compute a partial coverage report
# (c2.ckpt) on p.gpr's source files, however, ignore pkg-test.adb this time.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=test1, projects=["p"], units=["pkg"]),
    covlevel="stmt",
    gpr_obj_dir="obj-test1",
    gpr_exe_dir="obj-test1",
    mains=["main"],
    ignored_source_files=["pkg-test.adb"],
    extra_coverage_args=["--save-checkpoint=c2.ckpt"],
    out="log-cov2.txt",
)
check_empty("log-cov2.txt")

# Consolidate both partial reports and produce an XCOV report
xcov(
    [
        "coverage",
        "-P",
        p,
        "--projects",
        "p",
        "-cstmt",
        "-axcov",
        "--output-dir=report",
        "--quiet",
        "-Cc1.ckpt",
        "-Cc2.ckpt",
    ],
    out="log-cons.txt",
)
check_empty("log-cons.txt")
check_xcov_reports(
    "report",
    {
        "main.adb.xcov": {"+": {5}},
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {2}},
        "pkg-test.adb.xcov": {"-": {8, 9}},
    },
)

thistest.result()
