from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_file

wd = Wdir()

wd.to_subdir("tmp_1")
TestCase(category=None).run(
    covcontrol=CovControl(
        ulist_in="../" + list_to_file(["ops*"]),
        units_out=["ops.orelse"],
        xreports=["ops.ads", "ops.adb", "ops-andthen.ads", "ops-andthen.adb"],
    )
)

thistest.result()
