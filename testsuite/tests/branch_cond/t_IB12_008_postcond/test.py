from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker("checkx.adb", options="-gnata -save-temps").run()
thistest.result()
