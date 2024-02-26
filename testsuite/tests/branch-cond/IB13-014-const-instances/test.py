from SUITE.context import thistest
from SCOV.map import MapChecker


MapChecker(
    sources=["services.adb", "test_services.adb"],
    execs=["test_services"],
    alis="obj/services.ali",
).run()
thistest.result()
