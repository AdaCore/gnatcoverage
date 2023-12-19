"""
Check that "gnatcov setup" does not emit warnings about missing object/library
directories when loading the "gnatcov_rts.gpr" project file.
"""

import os
import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import xcov


tmp = Wdir("tmp_")

filename = "setup.log"
xcov(["setup", "--prefix=."], out=filename, force_project_args=True)


# gnatcov's "-q" option was introduced in 25.0w and was not ported to 24.1:
# manually post-process the output of "gnatcov setup" to discard
# "uninteresting" lines.
def to_ignore(line):
    for prefix in [
        "   [Ada]",
        "   [C]",
        "   [archive]",
        "   [gprlib]",
        "   [index]",
        "   [link library]",
        "   [mkdir]",
    ]:
        if line.startswith(prefix):
            return True
    result = line.strip() in {
        "Build Libraries",
        "Compile",
        "Setup",
        "Install project GNATcov_RTS",
        "Install project GNATcov_RTS - static",
        "Install project GNATcov_RTS - static-pic",
        "Install project GNATcov_RTS - relocatable",
    }
    return result


output = [line for line in lines_of(filename) if not to_ignore(line)]

thistest.fail_if_not_equal("gnatcov setup output", "", "\n".join(output))
thistest.result()
