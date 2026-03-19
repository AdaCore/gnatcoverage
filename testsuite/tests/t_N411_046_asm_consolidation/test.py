# -*- coding: utf-8 -*-

"""
Check that if GNATcov is provided twice the same trace file, it consolidates as
expected an assembly-defined procedure assembled with -g (it has a compile
unit DIE, but no subprogram DIE in the DWARF info).
"""

import os.path
import shutil

from OCOV.tc import TestCase
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor, tracename_for


test_drivers = {
    "test": {"p.s": ["-g"]},
}

coverage_expectations = {
    # There is only one "p" routine and we should consolidate them: we expect
    # only one "+" covered "p" routine.
    "p": {"-": 0, "!": 0, "+": 1},
}

# Generate a project just so that we use the C toolchain in order to generate
# the assembly sources (-save-temps). Make sure we do not generate debug info
# in the assembly code (-g0).
tmp = Wdir("tmp_asm")
prj = gprfor(mains=[], srcdirs=[".."], langs=["C"])
gprbuild(prj, gargs=["-c", "-u", "p.c"], extracargs=["-save-temps", "-g0"])
tmp.to_homedir()

# Run the consolidation check
tmp = Wdir("tmp_")
shutil.copy(os.path.join("..", "test.c"), "test.c")
shutil.copy(os.path.join("..", "tmp_asm", "obj", "p.s"), "p.s")

TestCase(
    test_drivers,
    coverage_expectations,
    extra_xcov_args=[tracename_for("test")],
).run()
thistest.result()
