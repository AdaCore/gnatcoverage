"""
Check that "gnatcov setup" displays a summary of env updates needed after a
successful run.
"""

import os.path
import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import xcov


tmp = Wdir("tmp_")

filename = "setup.log"
xcov(["setup", "--prefix=foo"], out=filename)
output = contents_of(filename)

output_re = re.compile(
    "(.|\n)*"
    "\nThe coverage runtime has been successfully installed."
    "\nIt was installed in: foo"
    "\nIn order to use it, remember to add:"
    "\n"
    "\n  (?P<gpr_dir>.*)"
    "\n"
    "\nto the GPR_PROJECT_PATH environment variable, and"
    "\n"
    "\n  (?P<shared_lib_dir>.*)"
    "\n"
    "\nto the (?P<shared_lib_var>.*) environment variable."
)
m = output_re.match(output)
thistest.fail_if_no_match("gnatcov setup output", output_re.pattern, output)
if m is not None:
    if thistest.env.build.os.name == "windows":
        shared_lib_dirname = "bin"
        shared_lib_var = "PATH"
        shared_lib_filename = "libgnatcov_rts.dll"
    else:
        shared_lib_dirname = "lib"
        shared_lib_var = "LD_LIBRARY_PATH"
        shared_lib_filename = "libgnatcov_rts.so"

    gpr_dir = m.group("gpr_dir")
    shared_lib_dir = m.group("shared_lib_dir")

    # Check what gnatcov prints
    thistest.fail_if_not_equal(
        "Mentionned GPR directory",
        os.path.realpath("foo/share/gpr"),
        os.path.realpath(gpr_dir),
    )
    thistest.fail_if_not_equal(
        "Mentionned shared lib directory",
        os.path.realpath(f"foo/{shared_lib_dirname}"),
        os.path.realpath(shared_lib_dir),
    )
    thistest.fail_if_not_equal(
        "Mentionned shared lib env var",
        shared_lib_var,
        m.group("shared_lib_var"),
    )

    # Sanity check: the mentionned directories should contain the expected
    # files.
    thistest.fail_if(
        not os.path.exists(os.path.join(gpr_dir, "gnatcov_rts.gpr")),
        f"Cannot find {filename}",
    )
    if not thistest.env.is_cross:
        thistest.fail_if(
            not os.path.exists(
                os.path.join(shared_lib_dir, shared_lib_filename)
            ),
            f"Cannot find {filename}",
        )

# Note that we cannot test here the summary when --prefix is not passed, since
# that would install gnatcov_rts in the toolchain prefix, which the testsuite
# is not allowed to do.

thistest.result()
