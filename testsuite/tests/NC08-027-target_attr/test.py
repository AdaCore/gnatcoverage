"""
Alternate test for NC08-027, complementary to the one within the GPR subset of
our suite.
"""

import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, indent
from SUITE.tutils import exepath_to, gprbuild, gprfor, xrun


# Work in a subdirectory
wd = Wdir(subdir="tmp_")

# Build a program expected to run fine for our current target
gpr = gprfor(mains=["foo.adb"], srcdirs=[".."])
gprbuild(project=gpr)

program_path = exepath_to("foo")

# Check that a Target attribute in a project file is accounted for
# by gnatcov run.
#
# Try to run with a project file containing a fake Target value.
# Expect an error complaining about an unknown target.
gpr = gprfor(mains=[], srcdirs=[".."], extra='for Target use "foo-bar";\n')
runargs = ["-P", gpr, program_path]
p = xrun(
    runargs,
    register_failure=False,
    auto_config_args=False,
    auto_target_args=False,
)

if thistest.bits() == 32:
    thistest.fail_if(
        not re.search(
            "No builtin or GNATemulator execution driver found for"
            " target: foo-bar",
            p.out,
        ),
        "couldn't find indication of unknown target error in gnatcov"
        " run output:\n {}".format(indent(p.out)),
    )
else:
    # thistest.bits() == 64
    # In that case, as gnatcov32 is picked by default if the given target
    # is invalid, it will fail when loading the executable file.

    thistest.fail_if(
        not re.search("unsupported ELF class", p.out),
        "couldn't find indication of ELF class unsupported error in gnatcov"
        " run output:\n {}".format(indent(p.out)),
    )

# Check that an explicit --target on the command line overrides the
# Target project file attribute.
#
# Same arguments as in the previous test, plus correct --target and --RTS
# arguments. This one should succeed.
xrun(runargs, auto_config_args=False, auto_target_args=True)

thistest.result()
