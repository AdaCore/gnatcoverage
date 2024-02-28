from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(extracargs="-ffunction-sections", category=CAT.stmt).run()
thistest.result()
