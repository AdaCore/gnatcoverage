"""
Check that two homonym C sources get distinct coverage buffer symbols when the
projects owning them have names that differ only by a dot.

gnatcov names those symbols after the owning project and the source simple
name. The C spelling of a project name joins the identifiers of a qualified
name with an underscore, so "p.child" and "p_child" both spell "p_child": used
as is, it would give both foo.c the same buffer symbol, and gnatcov would stop
on an internal error while collecting the buffers to dump.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

# Empty dummy project, just to make p.child a legal project name
gprfor(srcdirs=[], prjid="p", mains=[], objdir="obj-p")

# The two project names are the point of this test: keep them spelled so that
# joining the identifiers of the first one with an underscore yields the
# second one.
dotted = gprfor(
    prjid="p.child", srcdirs=["../src1"], mains=[], objdir="obj-dotted"
)
underscored = gprfor(
    prjid="p_child", srcdirs=["../src2"], mains=[], objdir="obj-underscored"
)

main_gpr = gprfor(
    prjid="main",
    mains=["main.c"],
    srcdirs=[".."],
    deps=[dotted, underscored],
    objdir="obj",
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=main_gpr),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    trace_mode="src",
)

# Both foo.c must have their own coverage buffers: were they to share one, the
# link would fail on a duplicate symbol, or one of them would report no
# coverage at all.
check_xcov_reports(
    "obj",
    {
        "main.c.xcov": {"+": {7, 13, 14}},
        "src1-foo.c.xcov": {"+": {6}},
        "src2-foo.c.xcov": {"+": {6}},
    },
)

thistest.result()
