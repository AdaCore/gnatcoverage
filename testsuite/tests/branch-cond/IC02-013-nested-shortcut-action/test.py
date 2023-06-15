from SCOV.map import MapChecker
from SUITE.context import thistest


MapChecker("check.adb", options="-gnatp").run()
MapChecker("check.adb").run()
thistest.result()
