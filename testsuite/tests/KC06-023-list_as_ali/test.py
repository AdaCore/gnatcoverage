from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.tutils import gprbuild, gprfor, xcov


Wdir("tmp_")
gprbuild(gprfor(["p.adb"], srcdirs="../src"))

# Expect silent success
xcov(["map-routines", "--scos=@../plist", "obj/p.o"])

# Expect failure
p = xcov(
    ["map-routines", "--scos=../plist", "obj/p.o"],
    out="list_as_ali.out",
    register_failure=False,
)
thistest.fail_if(
    p.status == 0 or not match("malformed ALI file", "list_as_ali.out"),
    "no error on malformed ALI file",
)
thistest.result()
