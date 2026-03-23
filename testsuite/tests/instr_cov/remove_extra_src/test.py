"""
Test that "gnatcov instrument" removes "$project_name-gnatcov-instr"
directories for projects that are not of interest.

Create a hierarchy of three projects: a library (mylib), a dummy project
(dummy) and a program using them (myprog). First instrument only the library
(to produce the "$project_name-gnatcov-instr" directory in the library's object
directory). Then, instrument only the program.

The latter is supposed to remove the library's "$project_name-gnatcov-instr"
directory. If it does not, building the program will trigger link errors:
gprbuild will blindly use the library's instrumented units but the instrumented
main will not pull the buffer units.

The dummy project has the same object directory as myprog. This used to make
gnatcov remove myprog's all instrumented source files, so the presence of dummy
in the tree of projects is here to check this does not happen anymore.
"""

import os.path

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


Wdir("tmp_")

# Create projects
mylib_gpr = gprfor(
    prjid="mylib",
    mains=[],
    langs=["Ada"],
    srcdirs=["../src-mylib"],
    objdir="obj-mylib",
)
dummy_gpr = gprfor(
    prjid="dummy",
    mains=[],
    langs=[],
    srcdirs=["../src-dummy"],
    objdir="obj-myprog",
)
myprog_gpr = gprfor(
    prjid="myprog",
    mains=["prog.adb"],
    srcdirs=["../src-myprog"],
    objdir="obj-myprog",
    deps=["mylib", "dummy"],
)

# Instrument mylib.gpr
xcov_instrument(
    gprsw=GPRswitches(root_project=mylib_gpr),
    covlevel="stmt",
    gpr_obj_dir="obj-mylib",
    out="instr-mylib.log",
)

# Create a non-empty directory in the mylib-gnatcov-instr folder, to check that
# the removal machinery handles it well (ignores it).
subdir = os.path.join("obj-mylib", "mylib-gnatcov-instr", "foo")
os.mkdir(subdir)
with open(os.path.join(subdir, "foo.txt"), "w") as f:
    pass

# Instrument, build, run and generate a coverage report for myprog.gpr (and not
# mylib.gpr).
build_run_and_coverage(
    gprsw=GPRswitches(root_project=myprog_gpr, no_subprojects=True),
    covlevel="stmt",
    mains=["prog"],
    gpr_obj_dir="obj-myprog",
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports("xcov", {"prog.adb.xcov": {"+": {5}, "-": {6}}})

thistest.result()
