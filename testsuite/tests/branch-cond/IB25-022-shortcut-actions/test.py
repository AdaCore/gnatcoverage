from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker("services.adb").run()
thistest.result()
