from SUITE.context import thistest
from SUITE.tutils import exename_for
from SCOV.map import MapChecker


MapChecker("checkx.adb", options="-gnata -save-temps").run()
MapChecker(
    sources=["services.adb", "test_services.ads"],
    options="-gnata",
    execs=[exename_for("test_services")],
    alis=["obj/services.ali"],
).run()
thistest.result()
