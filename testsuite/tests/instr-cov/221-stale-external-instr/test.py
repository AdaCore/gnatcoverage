"""
Check that "gnatcov instrument" warns about instrumented sources found in
externally built projects that are not of interest.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprfor


tmp = Wdir("tmp_")

# Create the "lib" project, that is externally built by default, and a project
# "p" that depends on "lib".
lib = gprfor(
    prjid="lib",
    srcdirs=["../src-lib"],
    objdir="obj-lib",
    mains=[],
    extra="""
        type Boolean is ("False", "True");
        Ext_Built : Boolean := external ("LIB_EXT_BUILT", "True");
        for Externally_Built use Ext_Built;
    """,
)
p = gprfor(
    prjid="p",
    deps=["lib"],
    srcdirs=["../src-p"],
    objdir="obj-p",
    mains=["main.adb"],
)

# Instrument and build the "lib" project
thistest.log("== Instrument Lib ==")
xcov_instrument(
    gprsw=GPRswitches(root_project=lib),
    covlevel="stmt",
    gpr_obj_dir="obj-lib",
    extra_args=["-XLIB_EXT_BUILT=False"],
    out="instr-lib.txt",
)
thistest.fail_if_not_equal(
    '"gnatcov instrument" output',
    "",
    contents_of("instr-lib.txt"),
)
gprbuild(lib, gargs=["-XLIB_EXT_BUILT=False"], out="gprbuild-lib.txt")

# Instrument the "p" project. We expect gnatcov to notice and warn that there
# are instrumented sources in "lib" even though it is not a project of
# interest.
thistest.log("== Instrument P ==")
xcov_instrument(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt",
    gpr_obj_dir="obj-p",
    out="instr-p.txt",
    tolerate_messages=".",
)
thistest.fail_if_not_equal(
    '"gnatcov instrument" output',
    'warning: Project "lib" is externally built and does not contain units of'
    " interest, however it contains instrumented sources",
    contents_of("instr-p.txt").strip(),
)

# Sanity check that the warning is useful:
#
# Instrumentation of main.adb considered that sources from "lib.gpr" were not
# of interest, so auto-dumping code does not reference coverage buffer units
# for "lib.gpr". As a result, these coverage buffers are not included in the
# build closure.
#
# However, sources for "lib.gpr" were instrumented and thus try to use coverage
# buffers, so we expect a link failure here.
thistest.log("== Build P ==")
gprbuild_desc = gprbuild(p, out="gprbuild-p.txt", register_failure=False)
thistest.fail_if(
    gprbuild_desc.status == 0,
    "build of P was expected to fail",
)
thistest.fail_if(
    "undefined reference to" not in contents_of("gprbuild-p.txt"),
    "Expected link error message not found in gprbuild-p.txt",
)

thistest.result()
