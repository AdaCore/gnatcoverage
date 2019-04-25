import os.path

from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest

libdep = os.path.abspath('mylib/mylib.gpr')
TestCase().run(covcontrol=CovControl(deps=[libdep],
                                     scoptions='-Pgen --recursive'))
thistest.result()
