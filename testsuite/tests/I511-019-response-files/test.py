from SUITE.context import thistest
from SUITE.cutils import Wdir, set_from
from SUITE.tutils import gprbuild, gprfor, xcov


Wdir("tmp_")

# Build a dummy project, to generate objects that we will query with
# disp-routines via an explicit list or a response file, to compare
# the results.
gprbuild(gprfor(["test_foobar.adb"], srcdirs="../src"))
objects = ["obj/foo.o", "obj/bar.o"]

# Get disp-routines output from explicit command line arguments
xcov(args=["disp-routines"] + objects, out="explicit.out")

# Build response file with same list of objects, and get disp-routines
# output from this
with open("foobar.list", "w") as f:
    f.write("\n".join(objects) + "\n")

xcov(args=["disp-routines", "@foobar.list"], out="response.out")

# Check that both outputs are the same and non-empty, with at least one
# of the bits we expect there. Do not rely on symbols ordering: it is not
# specified.
response = set_from("response.out")
explicit = set_from("explicit.out")
thistest.fail_if(
    explicit != response,
    "expect disp-routines output identical from args or response file",
)
thistest.fail_if(
    not any("bar" in symbol for symbol in explicit),
    'expect "bar" in disp-routines output',
)
thistest.result()
