import os.path

from SCOV.minicheck import build_run_and_coverage
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


wd = Wdir()


def try_one_gpr(gpr, no_such):
    label = os.path.basename(os.getcwd())
    dump = "xcov.out"

    build_run_and_coverage(
        gprsw=GPRswitches(root_project=gpr),
        covlevel="stmt",
        mains=["p"],
        extra_coverage_args=["-axcov"],
        out=dump,
        register_failure=False,
    )
    dump = contents_of(dump)

    expected_warning = (
        "no unit {} in project gen (coverage.units attribute)".format(no_such)
        if no_such
        else "no unit of interest"
    )

    thistest.fail_if(
        expected_warning not in dump,
        "[{}] missing warning on absence of specified unit".format(label),
    )


# Empty by specifying a single, non-existing unit in only
wd.to_subdir("tmp_1")
try_one_gpr(
    gpr=gprfor(
        srcdirs="../src",
        mains="p.adb",
        extra=CovControl(units_in=["no_such_unit"]).gpr(
            units_from_test_driver=[]
        ),
    ),
    no_such="no_such_unit",
)

# Empty by excluding the only candidate unit
wd.to_subdir("tmp_2")
try_one_gpr(
    gpr=gprfor(
        srcdirs="../src",
        mains="p.adb",
        extra=CovControl(units_out=["p"]).gpr(units_from_test_driver=[]),
    ),
    no_such=None,
)

# Empty by including the empty set explicitly
wd.to_subdir("tmp_3")
try_one_gpr(
    gpr=gprfor(
        srcdirs="../src",
        mains="p.adb",
        extra=CovControl(units_in=[]).gpr(units_from_test_driver=[]),
    ),
    no_such=None,
)

thistest.result()
