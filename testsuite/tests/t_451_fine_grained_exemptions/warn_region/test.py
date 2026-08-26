"""
Check that gnatcov warns about fine grained exemptions that are covered by
exemption regions.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import Exempt_Off, Exempt_On, generate_annotations, gprfor


tmp = Wdir("tmp_")

main_adb = os.path.abspath("../main.adb")
annotations = generate_annotations(
    [
        Exempt_On(main_adb, "4:1", "4:1", "J1"),
        Exempt_Off(main_adb, "7:17", "7.17"),
    ]
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=[
        "--annotate=xcov",
        "--output-dir=xcov",
        "--external-annotations",
        annotations,
    ],
    tolerate_coverage_messages=".*",
)

thistest.fail_if_not_equal(
    '"gnatcov coverage" output',
    "*** main.adb:7:10: warning: Ignoring exemption at main.adb:7:10 for"
    " outcome FALSE as it is already covered by the exemption region starting"
    " at main.adb:4:1",
    contents_of("coverage.log").strip(),
)

check_xcov_reports(
    "xcov", {"main.adb.xcov": {"+": {8, 12}, "*": {4, 5, 6, 7}}}
)

thistest.result()
