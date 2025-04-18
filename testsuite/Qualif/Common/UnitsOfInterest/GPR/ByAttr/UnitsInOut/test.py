from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir


wd = Wdir()

# Units = (In or All) - Out, order independant

# All (no In) is another testcase
# Out empty (In only) out is another testcase

# Out overlaps In
wd.to_subdir("tmp_1")
TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops", "ops.andthen"],
        units_out=["ops.andthen"],
        xreports=["ops.ads", "ops.adb"],
    )
)

TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops", "ops.andthen", "ops.orelse"],
        units_out=["ops", "ops.andthen"],
        xreports=["ops-orelse.ads", "ops-orelse.adb"],
    )
)

# Out does not overlap In
wd.to_subdir("tmp_2")

TestCase(category=None).run(
    covcontrol=CovControl(
        units_in=["ops", "ops.orelse"],
        units_out=["ops.andthen"],
        xreports=["ops.ads", "ops.adb", "ops-orelse.ads", "ops-orelse.adb"],
    )
)

thistest.result()
