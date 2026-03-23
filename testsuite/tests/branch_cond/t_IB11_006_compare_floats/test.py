from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker("inrange.adb").run()
thistest.result()
