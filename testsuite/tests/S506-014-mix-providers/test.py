"""
Check that gnatcov properly warns when a unit is processed both with
instrumentation and regular SCOs.

Also check that passing both kind of traces is rejected by gnatcov.
"""

from e3.fs import cp, mkdir

from SCOV.minicheck import build_and_run
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov

import re


tmp = Wdir("tmp_")
prj = gprfor(srcdirs=[".."], mains=["main.adb"])


def check(tc_name, coverage_args, expected_results, expect_failure):
    p = xcov(
        ["coverage", "-P", prj, "-axcov", "-cstmt"] + coverage_args,
        out=f"{tc_name}.log",
        register_failure=not expect_failure,
    )
    if expect_failure:
        thistest.fail_if(
            p.status == 0, "gnatcov did not exit with an error ({tc_name})"
        )

    actual = "\n".join(
        line.rstrip() for line in contents_of(f"{tc_name}.log").splitlines()
    )

    thistest.fail_if_no_match(
        f"unexpected gnatcov output ({tc_name})",
        regexp=expected_results,
        actual=actual,
    )


# First, build and run the program in src-traces mode to produce SIDs and a
# source trace.
src_trace = build_and_run(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    extra_coverage_args=[],
    mains=["main"],
    extra_gprbuild_args=["-XBUILD_MODE=src-traces"],
    trace_mode="src",
)[-1]

# The build_and_run invocation bellow implicitly invokes gprclean which will
# delete the SID files, so we need to copy them elsewhere to avoid having them
# be deleted.
sids_dir = "sids"
mkdir(sids_dir)
cp("obj/*.sid", sids_dir)

# Second, build and run the program to produce SCOs (ALIs) and a binary trace
bin_trace = build_and_run(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    extra_coverage_args=[],
    mains=["main"],
    extra_gprbuild_args=[
        "-XBUILD_MODE=bin-traces",
        "-cargs",
        "-g",
        "-fdump-scos",
        "-fpreserve-control-flow",
    ],
    trace_mode="bin",
)[-1]

# We can now try to compute code coverage with both the instrumentation
# checpoint and the compiler SCOs. This requires passing --mix-trace-kind
# to gnatcov to allow mixing trace kinds.

expected_log = re.escape(
    "warning: Mixing source traces and binary traces is not supported. "
    "Please only use a single kind of traces."
    "\nwarning: ignoring duplicate SCOs for main.adb (from main.ali)"
    "\nwarning: previous SCOs for this unit came from instrumenting main.adb"
    "\nwarning: Using option --sid with binary trace files has no effect."
    "\nPlease consider using option --scos or -P<project file> in conjunction"
    " with --units to specify units of interest."
    "\nwarning: Using option --scos with source trace files has no effect."
    "\nPlease consider using option --sid or -P<project file> in conjunction"
    " with --units to specify units of interest."
)

check(
    "trace_mix",
    coverage_args=[
        "--mix-trace-kind",
        "--scos",
        "obj/main.ali",
        "--sid",
        f"{sids_dir}/main.sid",
        src_trace,
        bin_trace,
    ],
    expected_results=expected_log,
    expect_failure=False,
)

# Check that without --mix-trace-kind, providing traces of different kind
# results in an error.

check(
    "mix_not_allowed",
    coverage_args=[
        "--scos",
        "obj/main.ali",
        "--sid",
        f"{sids_dir}/main.sid",
        src_trace,
        bin_trace,
    ],
    expected_results=r".*gnatcov(\.exe)?: Mixing source traces and binary"
    r" traces is not supported\. Please only use a single kind of traces\.",
    expect_failure=True,
)

# Check that using --scos in source trace mode emits a warning.
# Also produce a checkpoint for later testing.

check(
    "scos_with_src",
    coverage_args=[
        "--scos",
        "obj/main.ali",
        "--sid",
        f"{sids_dir}/main.sid",
        "--save-checkpoint=src.ckpt",
        src_trace,
    ],
    expected_results=re.escape(
        "warning: Using option --scos with source trace files"
        " has no effect.\nPlease consider using option --sid or"
        " -P<project file> in conjunction with --units to specify units of"
        " interest."
    ),
    expect_failure=False,
)

# Check that using --sid with binary traces emits a warning.
# Also produce a checkpoint for later testing.

check(
    "sid_with_bin",
    coverage_args=[
        "--scos",
        "obj/main.ali",
        "--sid",
        f"{sids_dir}/main.sid",
        "--save-checkpoint=bin.ckpt",
        bin_trace,
    ],
    expected_results=re.escape(
        "warning: Using option --sid with binary trace files"
        " has no effect.\nPlease consider using option --scos or"
        " -P<project file> in conjunction with --units to specify units of"
        " interest."
    ),
    expect_failure=False,
)

# Check that mixing trace kinds through checkpoints is rejected

check(
    "mixed_checkpoint",
    coverage_args=["-Csrc.ckpt", "-Cbin.ckpt"],
    expected_results=r".*gnatcov(\.exe)?: Mixing source traces and binary"
    r" traces is not supported\. Please only use a single kind of traces\.",
    expect_failure=True,
)


thistest.result()
