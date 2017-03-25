from SUITE.tutils import *
from SUITE.cutils import Wdir
from SCOV.tc import *

tc = TestCase()

tc.run()
tc.run(
    covcontrol=CovControl (covoptions="-S instance"),
    subdirhint="i_")

thistest.result ()

