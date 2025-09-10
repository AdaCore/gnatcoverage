import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import exepath_to, gprbuild, gprfor, xcov, xrun


wd = Wdir("tmp_")

# We have two candidate main drivers. Craft a gpr
# with a Main attribute listing only the first one.

mainbase1 = "test_tt"
mainunit1 = mainbase1 + ".adb"
exe1 = exepath_to(mainbase1)

mainbase2 = "test_tf"
mainunit2 = mainbase2 + ".adb"
exe2 = exepath_to(mainbase2)

gprname = gprfor(srcdirs=["../../../../src", "../../src"], mains=[mainunit1])

# Build both executables, passing both main unit
# names on the command line:
gprbuild(project=gprname, gargs=[mainunit1, mainunit2])


# Arrange to gnatcov run providing either exe1, exe2 or
# no executable. In all cases, expect to find a trace with
# at least an entry showing actual execution of something
# for this particular case.


def check(explicit_exe):
    outbase = explicit_exe if explicit_exe else "noexe"
    trace = "%s.trace" % outbase
    dump = "%s.dt" % outbase

    runcmd = ["-P", gprname, "-o", trace]
    if explicit_exe:
        runcmd.append(explicit_exe)

    xrun(runcmd)
    xcov(["dump-trace", trace], out=dump)

    thistest.fail_if(
        len(re.findall("t block$", contents_of(dump), flags=re.M)) < 1,
        "with %s, no block trace entry found in %s" % (outbase, trace),
    )


# With an explicit exe1, we just confirm what the Main attribute
# would convey.
check(explicit_exe=exe1)

# With an explicit exe2, we override it
check(explicit_exe=exe2)

# Without an explicit exe name on the gnatcov run command line,
# we expect gnatcov to figure out what it should be out of the
# Main attribute.
check(explicit_exe=None)

thistest.result()
