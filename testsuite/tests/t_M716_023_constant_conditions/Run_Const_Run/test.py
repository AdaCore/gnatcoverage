from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


for cat in CAT.critcats:
    TestCase(category=cat).run()
thistest.result()
