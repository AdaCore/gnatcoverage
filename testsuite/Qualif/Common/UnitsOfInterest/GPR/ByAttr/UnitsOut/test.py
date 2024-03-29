from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir


base_out = ["test_or_ft", "test_and_tt", "test_and_tf"]
wd = Wdir()

# Check on lone node unit only
wd.to_subdir("wd_1")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_out=base_out + ["ops"],
        xreports=[
            "ops-andthen.ads",
            "ops-andthen.adb",
            "ops-orelse.ads",
            "ops-orelse.adb",
        ],
    )
)

# Check on child units only
wd.to_subdir("wd_2")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_out=base_out + ["ops.orelse", "ops.andthen"],
        xreports=["ops.ads", "ops.adb"],
    )
)

# Check on root + child unit
wd.to_subdir("wd_3")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_out=base_out + ["ops", "ops.andthen"],
        xreports=["ops-orelse.ads", "ops-orelse.adb"],
    )
)

thistest.result()
