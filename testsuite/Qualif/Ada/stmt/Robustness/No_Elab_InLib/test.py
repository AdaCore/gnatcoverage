import os.path

from e3.fs import rm

from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches

libdep = os.path.abspath("mylib/mylib.gpr")

# Cleanup artifacts from previous testsuite runs
rm("mylib/obj", recursive=True)
rm("mylib/lib", recursive=True)

TestCase().run(
    covcontrol=CovControl(
        deps=[libdep],
        gprsw=GPRswitches(
            root_project="gen.gpr",
            units=["foo", "bar", "klunk"],
            xvars=[("TRACE_MODE", thistest.options.trace_mode)],
        ),
    )
)
thistest.result()
