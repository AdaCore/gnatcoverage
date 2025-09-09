from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_tmp


# Mixing units and lists to include / exclude
wd = Wdir()
wd.to_subdir("tmp_1")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops", "ops.andthen"],
        ulist_out=list_to_tmp(["ops"]),
        xreports=["ops-andthen.ads", "ops-andthen.adb"],
    )
)

TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops", "ops.andthen"],
        units_out=["ops.andthen"],
        xreports=["ops.ads", "ops.adb"],
    )
)

thistest.result()
