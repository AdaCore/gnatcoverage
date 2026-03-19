from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker("starts.adb", options="-gnata").run()
MapChecker("starts.adb", options="-gnata -gnatp").run()
thistest.result()
