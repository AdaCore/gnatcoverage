from SCOV.map import MapChecker
from SUITE.context import thistest


MapChecker("p.adb", options="-gnatp").run()
MapChecker("p.adb").run()
thistest.result()
