"""
Check that the Ada instrumenter does not violate elaboration constraints in a
unit spec when instrumenting a subunit corresponding to that spec.
"""

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


pragma_pattern = "--  PRAGMA"
config_pragma_pattern = "--  CONFIG_PRAGMA"

# Check that gnatcov procedures valid code for a subunit (pkg-nested.adb) when
# that subunit implements a library-level package body. Test both for an
# elaboration pragma and for the No_Elaboration_Code restriction since their
# handling involves different code paths in gnatcov.
#
# Check that we can still compute code coverage for the elaboration of
# declaration in a package body subunit when it is not library level
# (proc-nested.adb). This is possible only if gnatcov knows that this package
# is not library-level: it is possible to insert witness calls even if the unit
# has elaboration restrictions.
for name, pattern, pragma in [
    (
        "no_elab_code",
        config_pragma_pattern,
        "pragma Restrictions (No_Elaboration_Code);",
    ),
    (
        "preelab",
        pragma_pattern,
        "pragma Preelaborate;",
    ),
    (
        "no_elab_code_all",
        pragma_pattern,
        "pragma No_Elaboration_Code_All;",
    ),
]:
    thistest.log(f"== {name}: {pragma} ==")
    tmp = Wdir(f"tmp_{name}")

    # Instantiate the project sources in the temporary directory
    for ext in ["adb", "ads"]:
        cp(f"../*.{ext}", ".")
    with open("pkg.ads", "w") as f:
        f.write(contents_of("../pkg.ads").replace(pattern, pragma))

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=["."], mains=["main.adb"])
        ),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["-axcov"],
    )

    expected = {
        "main.adb.xcov": {"+": {7, 8}, "-": {9, 10}},
        "pkg.ads.xcov": {},
        "pkg-nested.adb.xcov": {"+": {12, 21}},
        "pkg-proc.adb.xcov": {"-": {14, 15, 16}},
        "pkg-proc-nested.adb.xcov": {"-": {4, 12, 21}},
        "utils.ads.xcov": {},
        "utils.adb.xcov": {"-": {9}},
    }
    if pattern == config_pragma_pattern:
        expected.pop("pkg.ads.xcov")
    check_xcov_reports("*.xcov", expected, cwd="obj")

    tmp.to_homedir()

thistest.result()
