from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(
    extracargs=["-no-integrated-cpp", "-falign-functions=32"],
    category=CAT.decision,
).run()
thistest.result()
