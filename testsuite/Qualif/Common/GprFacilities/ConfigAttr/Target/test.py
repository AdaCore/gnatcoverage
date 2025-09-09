"""
Test that when a project file has a Target attribute, "gnatcov run" does not
need a --target argument.  Also check that --target arguments take precedence
over the Target attribute.
"""

import os.path
import re

from e3.env import Env

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xrun,
    xcov,
)


gprname = "p"
mainbase = "foo"
mainunit = mainbase + ".adb"

env = Env()
target = env.target.triplet

Wdir("tmp_")

# Get a template for the project file.
gpr_filename = gprfor(
    prjid=gprname,
    mains=[mainunit],
    srcdirs=["../.."],
    langs=["Ada"],
    extra='for Target use "%TARGET%"; %RUNTIME%',
)
gpr_filename = os.path.abspath(gpr_filename)
gpr_basename = os.path.basename(gpr_filename)
gpr_content = contents_of(gpr_filename)


def instantiate_gpr(target):
    with open(gpr_basename, "w") as f:
        content = gpr_content.replace("%TARGET%", target)
        content = content.replace(
            "%RUNTIME%",
            (
                'for Runtime ("Ada") use "{}";'.format(thistest.options.RTS)
                if thistest.options.RTS
                else ""
            ),
        )
        f.write(content)


for mode in ("no_arg", "with_arg"):
    wd = Wdir("tmp_{}".format(mode))

    exe = exepath_to(mainbase)
    trace = tracename_for(mainbase)

    # Build with the real target as the Target attribute.
    instantiate_gpr(target)
    gprbuild(os.path.abspath(gpr_basename))

    argv = ["-P{}".format(gprname), "-o", trace, exe]

    # Run with a bad target as the Target attribute in order to check that the
    # --target argument actually takes precedence.
    with_target_arg = mode == "with_arg"
    if with_target_arg:
        instantiate_gpr("this_target_does_not_exist")

        # Force the passing of --target in the native case, as xrun() does not
        # pass it when it is the default.
        if not env.is_cross:
            argv.append("--target={}".format(target))

    xrun(argv, auto_config_args=False, auto_target_args=with_target_arg)

    dump = "dump.txt"
    xcov("dump-trace {}".format(trace), out=dump)
    thistest.fail_if(
        len(re.findall("t block$", contents_of(dump), flags=re.M)) < 1,
        "with exe, no block execution trace found in {}".format(trace),
    )

    wd.to_homedir()

thistest.result()
