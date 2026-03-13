"""
Check that the justification message for exemptions is visible as a message in
source based coverage reports. This was never verified by the testsuite as only
the line state is checked for source based reports. The =report format does
correctly display the justification, as tested by the SCOV test framework.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    contents_of,
    Exempt_Region,
    generate_annotations,
    gprfor,
)


def check_exemption_justification(
    report_dir: str, exempted_region: Exempt_Region
):
    """
    Check that we can locate a .xcov file in report_dir for the annotated file
    in exempted_region, and that it contains the expected justification message
    right after the line starting the annotation.
    """
    file_bn = os.path.basename(exempted_region.source_file)
    report_file = os.path.join(report_dir, file_bn + ".xcov")
    report_lines = contents_of(report_file).splitlines()
    ex_line = int(exempted_region.start_sloc.split(":")[0])
    index = 0
    while index < len(report_lines) and not report_lines[index].startswith(
        f"{ex_line:4} *:"
    ):
        index += 1
    thistest.fail_if(
        index >= len(report_lines) - 1,
        f"Could not find report line for line {ex_line} for {report_file}",
    )
    expected_message = (
        f"{file_bn}:{exempted_region.start_sloc}: Exempted region"
        f" justification: "
    )
    if exempted_region.justification:
        expected_message = (
            expected_message + f'"{exempted_region.justification}"'
        )
    else:
        expected_message = expected_message + "<NO JUSTIFICATION>"
    thistest.fail_if_not_equal(
        what="Unexpected message in lieu of exemption region justification",
        expected=expected_message,
        actual=report_lines[index + 1],
    )


tmp = Wdir("tmp_")
prj = gprfor(srcdirs=[".."], mains=["main.adb", "main_c.c"])
annotations = [
    Exempt_Region("../main.adb", "7:4", "8:46", "Ada justification text"),
    Exempt_Region("../main_c.c", "8:3", "9:14", "C justification text"),
    Exempt_Region("../static_lib.h", "4:3", "5:17", "C Header text"),
]
annotations_file = generate_annotations(annotations, prj)
build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    mains=["main", "main_c"],
    covlevel="stmt+decision",
    extra_coverage_args=[
        "-axcov+",
        f"--external-annotations={annotations_file}",
    ],
    tolerate_instrument_messages=(
        "main.adb:12:4: warning: No justification given for exempted region"
    ),
)
check_xcov_reports(
    reports_dir="obj",
    expected_cov={
        "main.adb.xcov": {"+": {4, 10}, "*": {7, 8, 12, 13, 14, 15, 16}},
        "main_c.c.xcov": {"+": {7, 10}, "*": {8, 9}},
        "static_lib.h.xcov": {"+": {6}, "*": {4, 5}},
    },
)

for exempted_region in annotations:
    check_exemption_justification("obj", exempted_region)

# Check the result of no exemption justification with an in-source annotation,
# as the add-annotation command rejects exemptions with no justifications.
check_exemption_justification("obj", Exempt_Region("main.adb", "12:4", "16:4"))

thistest.result()
