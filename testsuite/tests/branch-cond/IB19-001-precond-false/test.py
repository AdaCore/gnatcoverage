from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker("check.adb", options="-gnata -save-temps").run()
thistest.result()
