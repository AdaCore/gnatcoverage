"""
Check that "gnatcov instrument" correctly deals with directories for
instrumented sources in project extension configurations.
"""

import glob

from e3.fs import sync_tree

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Create a copy of the testcase material (project sources and dummy object
# directories) and run the instrumenter on the ultimate extending project.
sync_tree("../src", ".")
xcov_instrument(gprsw=GPRswitches(root_project="p_ext2.gpr"), covlevel="stmt")

# Check that directories for instrumented sources are all empty except for the
# ultimate extending project, which should contain all instrumented sources.
thistest.fail_if_not_equal(
    "instrumented directories",
    "\n".join([
        "obj-p_ext2/p_ext2-gnatcov-instr/gcvrt-bpkg.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/gcvrt-bpkg1.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/gcvrt-bpkg2.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/gcvrt-p_ext2.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/gcvrt-ppkg.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/gcvrt-ppkg1.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/gcvrt-ppkg2.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/pkg.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/pkg1.ads",
        "obj-p_ext2/p_ext2-gnatcov-instr/pkg2.ads",
    ]),
    "\n".join(
        f.replace("\\", "/")
        for f in sorted(glob.glob("obj-*/*-gnatcov-instr/*"))
    ),
)

thistest.result()
