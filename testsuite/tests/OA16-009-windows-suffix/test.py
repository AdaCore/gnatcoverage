import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor, xcov, xrun


wd = Wdir("tmp_")
gprbuild(gprfor(srcdirs=[".."], mains=["foo.adb"]))

# We want to check that "gnatcov run" will find the executable even though the
# input casing is different and even though it misses the ".exe" suffix.
#
# This test runs only on Windows, and binary traces support for Windows is not
# supported anymore, so executables produced by gprbuild by default never have
# the "exe" extension.
os.rename("foo", "Foo.Exe")
xrun("foo")

# Here, we also want to check that the default filename for the trace file is
# based on the actual filesystem name, not the name "gnatcov run" received in
# the command line.
xcov(
    [
        "coverage",
        "--level=stmt",
        "--annotate=xcov",
        "--scos=obj\\foo.ali",
        "Foo.Exe.trace",
    ]
)

thistest.fail_if(
    not os.path.exists("foo.adb.xcov"),
    '"gnatcov coverage" did not produce a XCOV report for the main source'
    " file.",
)

thistest.result()
