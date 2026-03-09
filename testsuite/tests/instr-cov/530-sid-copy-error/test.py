"""
Check that errors while copying SID files from the object directory to the
library directory are reported with details about why the copy failed.
"""

import os
import stat

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


Wdir("tmp_")


# Create the object directory and the target SID file and make them read-only
os.mkdir("lib")
with open("lib/mylib.sid", "w"):
    pass
os.chmod("lib/mylib.sid", stat.S_IREAD)
os.chmod("lib", stat.S_IREAD | stat.S_IEXEC)

p = xcov_instrument(
    gprsw=GPRswitches(
        gprfor(
            mains=[],
            srcdirs=[".."],
            extra="""
                for Library_Name use "mylib";
                for Library_Dir use "lib";
            """,
        ),
    ),
    covlevel="stmt",
    register_failure=False,
)

# Restore write permissions so that the testsuite framework can cleanup the
# temporary directory.
os.chmod("lib", stat.S_IREAD | stat.S_IWRITE | stat.S_IEXEC)
if os.path.exists("lib/mylib.sid"):
    os.chmod("lib/mylib.sid", stat.S_IREAD | stat.S_IWRITE)

thistest.fail_if(p.status == 0, '"gnatcov instrument" failure expected')

# Make sure the casing for "gnatcov.exe" is the same as in the filesystem:
# e3.os.fs may use the wrong casing on Windows.
from_filename = os.path.abspath("obj/mylib.sid")
to_filename = os.path.abspath("lib/mylib.sid")

thistest.fail_if_not_equal(
    '"gnatcov instrument" output',
    (
        f"gnatcov: Error while copying {from_filename} to {to_filename}:"
        " Permission denied\n"
        f'gnatcov: file "{to_filename}" could not be deleted: Permission'
        " denied\n"
    ),
    contents_of("instrument.log"),
)

thistest.result()
