from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest

tc = TestCase()
tc.run()
tc.run(covcontrol=CovControl(covoptions='-S instance'),
       subdirhint='i_')
thistest.result()
