from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest


TestCase().run(covcontrol=CovControl(covoptions="--non-coverable"))
thistest.result()
