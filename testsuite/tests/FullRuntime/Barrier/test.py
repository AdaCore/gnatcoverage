from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


for cat in (CAT.stmt, CAT.decision):
    TestCase(category=cat).run()
thistest.result()
