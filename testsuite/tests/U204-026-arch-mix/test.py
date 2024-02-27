"""
Check that "gnatcov coverage" can deal with SID and checkpoint files produced
in various cross-configuration scenarios.
"""

import glob
import os.path

from e3.env import Env
from e3.fs import cp, mkdir

from SCOV.instr import xcov_convert_base64
from SCOV.minicheck import check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import exename_for, gprbuild, gprfor, run_cov_program, xcov


src_dir = os.getcwd()
env = Env()
tmp = Wdir("tmp_")


def gen_project(name):
    """
    Generate in the current directory a project file with the given name for
    this testcases' sources. Return its filename as well as its object/exec
    directory (computed from the given name).
    """
    obj_dir = f"obj-{name}"
    p = gprfor(
        mains=["main_1.adb", "main_2.adb"],
        prjid=name,
        srcdirs=[".."],
        objdir=obj_dir,
        exedir=obj_dir,
    )
    return (p, obj_dir)


def check_reports(xcov_dir):
    """
    Check that the "*.xcov" coverage reports in the given directory have the
    expected content. This assumes coverage data from the execution of both
    mains.
    """
    check_xcov_reports(
        xcov_dir,
        {
            "main_1.adb.xcov": {"+": {5}},
            "main_2.adb.xcov": {"+": {5}},
            "pkg.adb.xcov": {"+": {12, 14}, "!": {11}},
            "pkg.ads.xcov": {},
        },
        discard_empty=False,
    )


# Checkpoints were produced in a specific location both on Windows and Linux.
# Use the appropriate source rebasing arguments so that gnatcov can find all
# source files in the testcase's directory.
src_rebase_args = [
    f"--source-rebase=c:\\tmp\\u204-026-arch-mix={src_dir}",
    f"--source-rebase=/tmp/U204-026-arch-mix={src_dir}",
]
cov_args = ["coverage", "-cstmt+mcdc", "-axcov"] + src_rebase_args


#
# 1. Check that we can instrument on one host and build+run+coverage on another
#

# The instrumentation part was done in "gen.sh". Restore SIDs and instrumented
# sources for the two cases to test and do the build+run+coverage.
for name, gen_dir_name in [("win", "x86_64-windows"),
                           ("linux", "arm-elf-linux")]:
    thistest.log(f"== 1. {name} ==")

    # Generate a dedicated project for this
    p, obj_dir = gen_project(name)

    instr_dir = os.path.join(obj_dir, f"{name}-gnatcov-instr")
    mkdir(obj_dir)
    mkdir(instr_dir)

    # Copy SIDs and instrumented sources where expected in its object directory
    gen_dir = os.path.join("..", "gen", gen_dir_name)
    cp(os.path.join(gen_dir, "*.sid"), obj_dir)
    cp(os.path.join(gen_dir, "*.ad*"), instr_dir)

    # Build and run the instrumented program, collecting source traces
    traces = []
    gprbuild(p, trace_mode="src")
    for main in ["main_1", "main_2"]:
        out_file = f"{name}-{main}-out.txt"
        run_cov_program(
            executable=exename_for(os.path.join(obj_dir, main)), out=out_file
        )

        trace_file = f"{name}-{main}.srctrace"
        xcov_convert_base64(out_file, trace_file)
        traces.append(trace_file)

    # Finally, generate and check the coverage report from these traces
    xcov(cov_args + ["-P", p] + traces)
    check_reports(obj_dir)


#
# 2. Check that we can mix checkpoints created from source traces even when
#    these source traces were produced for different targets, possibly from
#    different hosts.
#

thistest.log("== 2. src-checkpoints mix ==")
mkdir("2-xcov")
xcov(
    cov_args
    + ["--output-dir=2-xcov"]
    + [f"--checkpoint={os.path.join('..', 'gen', ckpt)}"
       for ckpt in ["src-main_1.ckpt", "src-main_2.ckpt"]]
)
check_reports("2-xcov")


#
# 3. Reject mix of checkpoints created with binary traces for different
#    "bits-target" (32 vs 64-bit).
#

thistest.log("== 3. bin-checkpoints mix ==")
mkdir("3-xcov")
p = xcov(
    cov_args
    + ["--output-dir=3-xcov"]
    + [f"--checkpoint={os.path.join('..', 'gen', ckpt)}"
       for ckpt in ["bin-main_1.ckpt", "bin-main_2.ckpt"]],
    out="3-coverage-out.txt",
    register_failure=False,
)
thistest.fail_if(
    p.status == 0,
    "'gnatcov coverage' exited with status code 0 while an error was expected"
)

# Since the checkpoint for main_1 was created for 32-bits while the checkpoint
# for main_2 was created for 64-bits, the error message depends on the target
# platform for which we run the testsuite.
if env.target.cpu.bits == 32:
    ckpt = "bin-main_2.ckpt"
    ckpt_bits = "64-bit"
    req_bits = "32-bit"
else:
    assert env.target.cpu.bits == 64
    ckpt = "bin-main_1.ckpt"
    ckpt_bits = "32-bit"
    req_bits = "64-bit"
thistest.fail_if_no_match(
    "'gnatcov coverage' output",
    f".*gnatcov.*: .*{ckpt} was created with {ckpt_bits} traces whereas the"
    f" selected target requires {req_bits} traces",
    contents_of("3-coverage-out.txt"),
)

#
# 4. Check that we can use source traces produced on a different host
#

thistest.log("== 4. src-traces mix ==")
mkdir("4-xcov")
sid_list = "sids.txt"
with open(sid_list, "w") as f:
    for sid in sorted(glob.glob(os.path.join("..", "gen", "*", "*.sid"))):
        f.write(os.path.abspath(sid))
        f.write("\n")
xcov(
    cov_args
    + ["--output-dir=4-xcov", f"--sid=@{sid_list}"]
    + [os.path.join("..", "gen", f"main_{i}.srctrace") for i in range(1, 3)],
    out="4-coverage-out.txt",
)
check_reports("4-xcov")

thistest.result()
