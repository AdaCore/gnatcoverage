from SCOV.map import MapChecker
from SUITE.context import thistest

MapChecker("starts.adb", options="-gnata -save-temps -gnatp").run()
thistest.result()
