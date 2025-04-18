from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_tmp


# Mixing units and lists to include
wd = Wdir()
wd.to_subdir("tmp_1")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops"],
        ulist_in=list_to_tmp(["ops.andthen"]),
        xreports=["ops.ads", "ops.adb", "ops-andthen.ads", "ops-andthen.adb"],
    )
)

thistest.result()
