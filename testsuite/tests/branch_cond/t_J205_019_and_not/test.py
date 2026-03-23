from SCOV.map import MapChecker
from SUITE.context import thistest


MapChecker("check.adb").run()
MapChecker("foo.adb").run()
MapChecker("foo2.adb").run()
MapChecker("foo3.adb").run()
thistest.result()
