from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_tmp


wd = Wdir()

# Remove a parent unit
wd.to_subdir("tmp_1")
TestCase(category=None).run(
    covcontrol=CovControl(
        ulist_in=list_to_tmp(["ops", "ops.andthen"]),
        ulist_out=list_to_tmp(["ops"]),
        xreports=["ops-andthen.ads", "ops-andthen.adb"],
    )
)

# Remove a child unit
wd.to_subdir("tmp_2")
TestCase(category=None).run(
    covcontrol=CovControl(
        ulist_in=list_to_tmp(["ops", "ops.andthen"]),
        ulist_out=list_to_tmp(["ops.andthen"]),
        xreports=["ops.ads", "ops.adb"],
    )
)

# Remove one that's not in
wd.to_subdir("tmp_3")
TestCase(category=None).run(
    covcontrol=CovControl(
        ulist_in=list_to_tmp(["ops", "ops.andthen"]),
        ulist_out=list_to_tmp(["ops.orelse"]),
        xreports=["ops.ads", "ops.adb", "ops-andthen.ads", "ops-andthen.adb"],
    )
)

thistest.result()
