"""
Test that we have no consolidation error in source coverage for two binaries
that bring different versions of the same separate procedure. The separate
procedure is part of an unit of interest, but the coverage campaign ignores the
separate itself.
"""

import os
import os.path
import shutil

from SCOV.minicheck import (
    build_run_and_coverage,
    checked_xcov,
    check_xcov_reports,
)
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import tracename_for, xcov


wd = Wdir("tmp_")


class Testcase(object):
    def __init__(self, name, objdir):
        self.name = name
        self._objdir = objdir

    @property
    def project_file(self):
        return os.path.join("..", "{}.gpr".format(self.name))

    @property
    def main(self):
        return "main_{}".format(self.name)

    def obj_dir(self, *args):
        return os.path.join("..", self._objdir, *args)

    def exe_dir(self, *args):
        return os.path.join("..", "bin", *args)

    @property
    def tracename(self):
        return tracename_for("main_{}".format(self.name))

    @property
    def checkpoint(self):
        return "{}.ckpt".format(self.name)


def clean_output_directory():
    if os.path.exists("output"):
        shutil.rmtree("output")
    os.mkdir("output")


test1 = Testcase("test1", "obj1")
test2 = Testcase("test2", "obj2")
testcases = [test1, test2]

covlevel = "stmt+decision"
extra_args = (["--units=pkg"],)
output_arg = "--output-dir=output"


# Build the test material (program and traces) and produce checkpoints
for testcase in testcases:
    thistest.log(f"== {testcase.name} ==")
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=testcase.project_file, units=["pkg"]),
        covlevel=covlevel,
        mains=[testcase.main],
        ignored_source_files=["pkg-test_driver.adb"],
        gpr_obj_dir=testcase.obj_dir(),
        gpr_exe_dir=testcase.exe_dir(),
        extra_coverage_args=["--save-checkpoint", testcase.checkpoint],
        out="ckpt-{}.log".format(testcase.name),
        scos_for_run=False,
    )

# When inlining is enabled, consolidation without checkpoints cannot work, even
# with --excluded-source-files.
if thistest.options.trace_mode == "bin" and "-O1" in thistest.suite_cargs_for(
    "Ada"
):
    p = xcov(
        [
            "coverage",
            "-c",
            covlevel,
            "--annotate=report",
            output_arg,
            test1.tracename,
            test2.tracename,
        ]
    )
    thistest.fail_if(
        p.status == 0,
        '"gnatcov coverage" is supposed to complain about different symbols'
        " during consolidation, but it did not.",
    )

# Check that consolidation behaves as expected with the --excluded-source-files
# switch: expected number of reports and expected report content.
clean_output_directory()
checked_xcov(
    [
        "coverage",
        "-c",
        covlevel,
        "--annotate=xcov",
        output_arg,
        "-C",
        test1.checkpoint,
        "-C",
        test2.checkpoint,
    ],
    "cons.log",
)

# Finally, check we have the expected reports
check_xcov_reports(
    "output",
    {"pkg.ads.xcov": {}, "pkg.adb.xcov": {"+": {5, 6, 8}}},
)
thistest.result()
