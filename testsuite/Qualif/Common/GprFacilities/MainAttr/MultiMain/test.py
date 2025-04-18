import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, match
from SUITE.tutils import exepath_to, gprbuild, gprfor, xcov, xrun


wd = Wdir("tmp_")

# GPR with multiple mains

gprname = "gen"
mainbases = ["test_tt", "test_tf"]
mainunits = [base + ".adb" for base in mainbases]

gprbuild(
    gprfor(
        prjid=gprname,
        srcdirs=["../../../../src", "../../src"],
        mains=mainunits,
    )
)

# We expect this to work. The multiple mains in the gpr file are just ignored
# when there is an exe on the command line.
exe = exepath_to("test_tt")
trace = "tt.trace0"
dump = "tt.dump0"

xrun(["-P", gprname, "-o", trace, exe])
xcov(["dump-trace", trace], out=dump)
thistest.fail_if(
    len(re.findall("t block$", contents_of(dump), flags=re.M)) < 1,
    "with exe, no block execution trace found in %s" % trace,
)

# Again, _not_ providing the executable. Expected to fail
# from missing command line argument.
trace = "oops.trace0"
dump = "oops.dump"
xrun(["-P", gprname, "-o", trace], out=dump, register_failure=False)

thistest.fail_if(
    not match(": Please specify an executable to run", dump),
    "missing expected error diag on main-less invocation of gnatcov run",
)

thistest.result()
