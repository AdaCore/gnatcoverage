from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest


TestCase(category=None).run(covcontrol=CovControl(auto_units=True))
thistest.result()
