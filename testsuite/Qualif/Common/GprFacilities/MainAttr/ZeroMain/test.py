import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import exepath_to, gprbuild, gprfor, xcov, xrun


wd = Wdir("tmp_")

# GPR without a main.
#
# We must provide the main executable on the command line.
gprname = "tt_nomain"
mainbase = "test_tf"
mainunit = mainbase + ".adb"
exe = exepath_to(mainbase)

gprbuild(
    gprfor(
        prjid=gprname,
        srcdirs=["../../../../src", "../../src"],
        mains="",
        langs=["Ada"],
    ),
    gargs=[mainunit],
)

trace = "tf.trace0"
dump = "tf.dump0"
xrun(["-P", gprname, exe, "-o", trace])
xcov(["dump-trace", trace], out=dump)
thistest.fail_if(
    len(re.findall("t block$", contents_of(dump), flags=re.M)) < 1,
    "with exe, no block execution trace found in %s" % trace,
)

thistest.result()
