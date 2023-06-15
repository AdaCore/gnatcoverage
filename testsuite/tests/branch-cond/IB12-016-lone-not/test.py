from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker(["eval_not.adb", "not_if.adb"], ensure_dcscos=False).run()
thistest.result()
