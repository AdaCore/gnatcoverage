import os.path
import re

from SUITE.cutils import Wdir, contents_of, indent
from SUITE.tutils import exepath_to, gprbuild, gprfor, thistest, xcov


src_dir = os.path.abspath(".")
wd = Wdir("tmp_")
exe_path = exepath_to("foo")
dmap_path = exe_path + ".dmap"


# Generate a project for our sources, build it and generate a decision map for
# foo.adb
prj_file = gprfor(["foo.adb"], srcdirs=src_dir, objdir=".")
gprbuild(prj_file)
xcov(["map-routines", "--scos=bar.ali", exe_path])
xcov(["dump-trace", dmap_path], out="dump-trace.txt")

dump_trace = contents_of("dump-trace.txt")
pattern = """\
Kind: DECISION_MAP

Traces:
[0-9a-f]+-[0-9a-f]+ \\?: 00 ----
.*"""
thistest.fail_if(
    not re.match(pattern, dump_trace, re.DOTALL),
    'Could not match "gnatcov dump-trace" output:\n'
    "{}\n"
    "against the expected pattern:\n"
    "{}\n".format(indent(dump_trace), indent(pattern)),
)

thistest.result()
