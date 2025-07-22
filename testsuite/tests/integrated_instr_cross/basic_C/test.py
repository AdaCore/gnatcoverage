"""
Simple sanity check test for integrated instrumentation for bareboard targets.
"""

import os
import os.path

from e3.fs import cp, mkdir

from SUITE.control import env, GPRBUILD
from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import (
    get_c_bsp,
    cmdrun,
    driver_for_lang,
    run_cov_program,
    thistest,
    xcov,
)

Wdir("tmp_")

# We need to setup the coverage runtime with no Ada support as otherwise we'll
# get undefined references to GNAT.IO

gcvrt_prefix = "gcvrt"
xcov(["setup", f"--prefix={gcvrt_prefix}", "--restricted-to-languages=C"])
env.add_search_path(
    "GPR_PROJECT_PATH",
    os.path.join(gcvrt_prefix, "share", "gpr"),
)

cp("../main.c", ".")

# Build the C BSP
bsp_root = "bsp_root"
mkdir(bsp_root)
bsp_prj_name = get_c_bsp(thistest.options.RTS, bsp_root)

bsp_prj = os.path.join(bsp_root, f"{bsp_prj_name}.gpr")

# Generate a simple project
main_prj = "prj.gpr"
with open(main_prj, "w") as prj_file:
    print(f'with "{bsp_prj}";', file=prj_file)
    print("project Prj is", file=prj_file)
    print(
        f'   for Target use "{thistest.env.target.platform}";',
        file=prj_file,
    )
    print('   for Languages use ("C");', file=prj_file)
    print('   for Source_Dirs use (".");', file=prj_file)
    print('   for Main use ("main.c");', file=prj_file)
    print('   for Object_Dir use "obj";', file=prj_file)
    print(
        f"   package Compiler renames {bsp_prj_name}.Compiler;",
        file=prj_file,
    )
    print("   package Linker is", file=prj_file)
    print(
        f'      for Switches ("C") use {bsp_prj_name}.Linker_Switches;',
        file=prj_file,
    )
    print("   end Linker;", file=prj_file)
    print("end Prj;", file=prj_file)

# Setup the integrated instrumentation
compiler = driver_for_lang("C")
gnatcov_artifact_dir = "gcv_artifacts"
xcov(
    [
        "setup-integration",
        "--level=stmt+mcdc",
        f"--files={os.path.abspath('main.c')}",
        f"--compilers={os.path.basename(compiler)}",
        f"--output-dir={gnatcov_artifact_dir}",
    ]
)
env.add_path(gnatcov_artifact_dir)

# Build our simple C main and execute it
cmdrun([GPRBUILD, "-P", main_prj], for_pgm=False)
run_log = "run.log"
run_cov_program("./obj/main", out=run_log)

trace_name = "main.srctrace"
xcov(["extract-base64-trace", run_log, trace_name])
xcov(
    [
        "coverage",
        "--sid",
        os.path.join(gnatcov_artifact_dir, "main.c.sid"),
        "-axcov",
        "-cstmt+mcdc",
        trace_name,
    ]
)
check_xcov_reports(
    reports_dir=".",
    expected_cov={
        "main.c.xcov": {"+": {7, 8, 9, 11, 14}, "!": {10}, "-": {13}},
    },
)

thistest.result()
