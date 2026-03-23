from SCOV.tc import TestCase, thistest
from SCOV.tctl import CAT

TestCase(category=CAT.stmt).run()
thistest.result()
