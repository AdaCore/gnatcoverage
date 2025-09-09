from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import gprcov_for

wd = Wdir()


def check(subdir, opspkg, xreports):
    wd.to_subdir(subdir)

    ops_gpr = gprfor(mains=[], prjid="ops", srcdirs="../src/ops", extra=opspkg)

    TestCase(category=None).run(
        covcontrol=CovControl(deps=[ops_gpr], units_in=[], xreports=xreports)
    )


# ops.gpr: no attribute

check(
    opspkg="",
    xreports=[
        "ops.adb",
        "ops.ads",
        "ops-andthen.ads",
        "ops-andthen.adb",
        "ops-orelse.ads",
        "ops-orelse.adb",
    ],
    subdir="tmp_1",
)

# ops.gpr: for Units use ...

check(
    opspkg=gprcov_for(units_in=["ops.andthen"]),
    xreports=["ops-andthen.ads", "ops-andthen.adb"],
    subdir="tmp_2",
)

# ops.gpr: for Excluded_Units use ...

check(
    opspkg=gprcov_for(units_out=["ops.andthen"]),
    xreports=["ops.ads", "ops.adb", "ops-orelse.ads", "ops-orelse.adb"],
    subdir="tmp_3",
)

# ops.gpr: for Units use ... for Excluded_Units use ...

check(
    opspkg=gprcov_for(
        units_in=["ops", "ops.orelse", "ops.andthen"], units_out=["ops.orelse"]
    ),
    xreports=["ops.ads", "ops.adb", "ops-andthen.ads", "ops-andthen.adb"],
    subdir="tmp_3",
)


thistest.result()
