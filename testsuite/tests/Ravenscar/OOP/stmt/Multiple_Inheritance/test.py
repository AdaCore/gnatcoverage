from SCOV.tc import TestCase
from SUITE.context import thistest


TestCase(extracargs=["-cargs:Ada", "-gnat05"]).run()
thistest.result()
