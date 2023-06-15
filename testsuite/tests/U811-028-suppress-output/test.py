"""
Check that --cancel-annotate prevents the emmission of a coverage
report.
"""

import os
from os import listdir
from os.path import exists

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, empty
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

p = gprfor(
  mains=["main.adb"],
  srcdirs=[".."],
  extra='package Coverage is'
        '\n   for Switches ("Coverage") use ("--annotate=report");'
        '\nend Coverage;\n'
)

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p),
    gpr_obj_dir="obj",
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=[],
)

# check that specifying --cancel-annotate without specifying
# --save-checkpoint is not allowed.

# As the gnatcov command is a full path by default on windows, we will
# explicitely specify it to avoid spurious diffs.
env = dict(os.environ)
env.update({"GNATCOV_CMD": "gnatcov"})

process = xcov(
    args=xcov_args + ["--cancel-annotate"],
    register_failure=False,
    out="no-save-checkpoint.txt",
    env=env,
)

thistest.fail_if(
    process.status == 0,
    comment="Gnatcov returned 0 with --cancel-annotate"
    " and no --save-checkpoint option specified",
)
thistest.fail_if_not_equal(
    what="Unexpected error message from gnatcov",
    expected="gnatcov: Please specify --save-checkpoint on the command line"
    " when --cancel-annotate is also specified.\n"
    "Usage: gnatcov coverage [OPTIONS] TRACE_FILEs\n"
    "Run 'gnatcov coverage --help' for more information.\n",
    actual=contents_of("no-save-checkpoint.txt"),
)

# Check that specifying --cancel-annotate suppresses the emission
# of a coverage report specified in the project file but a checkpoint
# is still emitted.

xcov(xcov_args + ["--cancel-annotate", "--save-checkpoint=prj_attr.ckpt"],
     out='suppress-report-project-file.txt')
thistest.fail_if(
    not empty("suppress-report-project-file.txt"),
    comment="report output not suppressed (from project file)"
)
thistest.fail_if(
    not exists("prj_attr.ckpt"),
    comment="Checkpoint file does not exist (project attribute)"
)


# Check that specifying --cancel-annotate with --annotate=report on the command
# line does suppress the report output.

xcov(
    args=xcov_args
    + [
        "--annotate=report",
        "--cancel-annotate",
        "--save-checkpoint=cmd_line_report.ckpt",
    ],
    out="suppress-report.txt",
)

thistest.fail_if(
    not empty("suppress-report.txt"),
    comment="report output not suppressed (from command line)"
)
thistest.fail_if(
    not exists("cmd_line_report.ckpt"),
    comment="Checkpoint file does not exists (--annotate=report)"
)

# Check that specifying --annotate=supress-output with --annotate=xcov on the
# command line does prevent the creation of .xcov files.

xcov(
    xcov_args
    + [
        "--annotate=xcov",
        "--output-dir=xcov",
        "--cancel-annotate",
        "--save-checkpoint=cmd_line_xcov.ckpt",
    ]
)

thistest.fail_if(
    len(listdir("xcov")) > 0,
    comment="xcov report produced with --cancel-annotate",
)
thistest.fail_if(
    not exists("cmd_line_xcov.ckpt"),
    comment="Checkpoint file does not exist (--annotate=xcov)"
)

thistest.result()
