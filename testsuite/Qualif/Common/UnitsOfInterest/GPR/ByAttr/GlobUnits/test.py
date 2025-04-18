from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir


wd = Wdir()

wd.to_subdir("tmp_1")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops*"],
        xreports=[
            "ops.ads",
            "ops.adb",
            "ops-andthen.ads",
            "ops-andthen.adb",
            "ops-orelse.ads",
            "ops-orelse.adb",
        ],
    )
)

wd.to_subdir("tmp_2")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_out=["test*"],
        xreports=[
            "ops.ads",
            "ops.adb",
            "ops-andthen.ads",
            "ops-andthen.adb",
            "ops-orelse.ads",
            "ops-orelse.adb",
        ],
    )
)

wd.to_subdir("tmp_3")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops*"],
        units_out=["ops.andthen"],
        xreports=["ops.ads", "ops.adb", "ops-orelse.ads", "ops-orelse.adb"],
    )
)

thistest.result()
