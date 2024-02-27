"""
Check that the instrumenter correctly renames qualified names in the mains.
This test mainly checks that the various mains used are correctly built after
instrumentation, and that their coverage results are correct.
This also checks the instrumentation of the main source correctly inserts
buffers dump code when the main procedure has top-level exception handlers.
(Former test S607-033-instr-exc-handler)
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir()

# Names of the various mains tested.
#
# Mains of the form "test_simple*"" are mains with a simple name
# (i.e. not dotted) whereas those of the form "test_dotted*" are mains with
# a dotted name.
#
# Mains of the form "*decl" are mains with top level declarations,
# those of the form "*exn" are mains with top level exception handlers,
# and should have their qualified names be modified.
#
# The mains of the form "*no_decl" have no top level decalrations nor exception
# handlers, and should not have their qualified names renamed.
main_names = [
    "simple_no_decl",
    "simple_decl",
    "simple_exn",
    "simple_only_exn",
    "dotted-decl",
    "dotted-no_decl",
    "dotted-exn",
]

# The expected results are not really of imporance here, what matters is that
# the mains got instrumented and build correctly.
expected_results = {
    "dotted-decl": {"dotted-decl.adb.xcov": {"+": {7, 12, 13, 17, 18}}},
    "dotted-no_decl": {"dotted-no_decl.adb.xcov": {"+": {6, 9, 10}}},
    "dotted-exn": {"dotted-exn.adb.xcov": {"+": {4, 9, 12, 13, 16}}},
    "simple_decl": {"simple_decl.adb.xcov": {"+": {10, 15, 16, 19, 20}}},
    "simple_no_decl": {"simple_no_decl.adb.xcov": {"+": {6, 9}}},
    "simple_exn": {"simple_exn.adb.xcov": {"+": {4, 9, 12, 13, 16}}},
    "simple_only_exn": {
        "simple_only_exn.adb.xcov": {"+": {10, 13, 14, 18}, "-": {20}}
    },
}


def do_one_main(main_name):
    """
    create a project, instrument build and compute coverage reports for the given
    main, from the expected results dictionnary defined above.
    """
    tmp.to_subdir(f"tmp_{main_name}")
    p = gprfor(mains=[f"{main_name}.adb"], srcdirs=[".."])
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=p, units=[main_name.replace("-", ".")]),
        covlevel="stmt",
        mains=[main_name],
        extra_coverage_args=["-axcov", "--output-dir=xcov"],
    )
    check_xcov_reports("xcov", expected_results[main_name])


for main_name in main_names:
    do_one_main(main_name)

thistest.result()
