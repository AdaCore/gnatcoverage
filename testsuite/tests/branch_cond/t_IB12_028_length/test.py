from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker("empty.adb", options="-fverbose-asm -save-temps").run()
thistest.result()
