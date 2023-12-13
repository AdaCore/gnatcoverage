"""
Check that "gnatcov setup" does not emit warnings about missing object/library
directories when loading the "gnatcov_rts.gpr" project file.
"""

import os
import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import xcov


tmp = Wdir("tmp_")

filename = "setup.log"
xcov(["setup", "--prefix=.", "-q"], out=filename, force_project_args=True)
thistest.fail_if_not_equal("gnatcov setup output", "", contents_of(filename))
thistest.result()
