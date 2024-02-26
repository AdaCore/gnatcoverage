import os
import os.path
import re
import shutil

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


wd = Wdir("tmp_")


def create_checkpoint(name, covlevel):
    gpr_exe_dir = "exe-{}".format(name)
    gpr_obj_dir = "obj-{}".format(name)
    prj_file = gprfor(
        mains=["test_{}.adb".format(name)],
        prjid=name,
        srcdirs=os.path.join("..", "src"),
        objdir=gpr_obj_dir,
        exedir=gpr_exe_dir,
        extra="package Coverage is\n"
        '   for Units use ("math");\n'
        "end Coverage;",
    )
    ckpt = "test_{}.ckpt".format(name)
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=prj_file),
        covlevel=covlevel,
        mains=["test_{}".format(name)],
        gpr_obj_dir=gpr_obj_dir,
        gpr_exe_dir=gpr_exe_dir,
        extra_coverage_args=["--save-checkpoint={}".format(ckpt)],
    )
    return ckpt


def clean_output_directory():
    if os.path.exists("output"):
        shutil.rmtree("output")
    os.mkdir("output")


# Create two checkpoints with different coverage levels
add_ckpt = create_checkpoint("add", "stmt")
mult_ckpt = create_checkpoint("mult", "stmt+decision")

# Consolidate both. The only attempt that is expected to suceed is the one
# with the weakest common coverage level criteria, i.e. stmt.
args = [
    "coverage",
    "--annotate=xcov",
    "--output-dir=output",
    "-C",
    add_ckpt,
    "-C",
    mult_ckpt,
]

# Attempting to consolidate them in stmt+decision mode should fail since one
# checkpoint was produced with a weaker coverage criteria (stmt).
# TODO: refine the expected error message.
clean_output_directory()
p = xcov(
    args + ["--level=stmt+decision"],
    "cons-stmt-decision.log",
    register_failure=False,
)
thistest.fail_if(
    p.status == 0
    or not re.match(
        r".*gnatcov.*: incompatible coverage level: .*test_add.*ckpt was"
        r' produced with "stmt" but we expect at least "stmt\+decision"',
        contents_of("cons-stmt-decision.log").strip(),
    ),
    "gnatcov did not complain about inconsistent coverage criteria across"
    " input checkpoints",
)

clean_output_directory()
xcov(args + ["--level=stmt"], "cons-stmt.log")

expected = {
    "math.adb.xcov": {"+": {7, 12, 13, 14, 17, 18, 19}},
    "math.ads.xcov": {},
}

# Finally, check we have the expected reports for the --level=stmt
# consolidation.
check_xcov_reports("output", expected, discard_empty=False)
thistest.result()
