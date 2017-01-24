from SUITE.tutils import thistest
from SCOV.tc import TestCase, CovControl

tc = TestCase()

tc.run()
tc.run(
    covcontrol=CovControl (covoptions="-S instance"),
    subdirhint="i_")

thistest.result ()

