import os.path

from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches

libdep = os.path.abspath('mylib/mylib.gpr')
TestCase().run(covcontrol=CovControl(
    deps=[libdep],
    gprsw=GPRswitches(root_project='gen.gpr', recursive=True)))
thistest.result()
