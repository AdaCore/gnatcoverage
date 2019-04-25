from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest

TestCase().run(CovControl(covoptions="--source-search=../../src"))
thistest.result()
