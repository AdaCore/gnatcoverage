import re

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import exepath_to, gprbuild, gprfor, xrun

Wdir("tmp_")

# Point of this test is to check that the execution of a program using the
# "system" function works fine, without triggering LD_PRELOAD warning from
# valgrind when this happens to be our underlying execution engine.

# We build and check the output of a program doing system("ls")

gprbuild(gprfor(mains=["sls.c"], srcdirs=".."))

p = xrun(exepath_to("sls"))

thistest.fail_if(
    re.search(pattern="LD_PRELOAD", string=p.out),
    "spurious occurrence of LD_PRELOAD in the gnatcov run output",
)

thistest.fail_if(
    not re.search(pattern="gen.gpr", string=p.out),
    "missing expected nls.c from the program output",
)

thistest.result()
