"""
Simple sanity checking test for the add-annotation, delete-annotation and
show-annotation commands.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

annotation_file = "annotations.toml"

# Create a simple project to work on
gprsw = GPRswitches(
    root_project=gprfor(
        srcdirs=[".."],
        mains=["main.adb"],
        langs=["Ada", "C"],
    )
)

# Instrument, build and run
cov_args = build_and_run(
    gprsw=gprsw,
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=[
        "-axcov",
        f"--external-annotations={annotation_file}",
    ],
)

# Generate some annotations. Only use exemption annotations as they can be
# interpreted at coverage time and do not require a full coverage cycle when
# modified.
# Pass the project file as argument to ensure the annotations only contain the
# required
xcov(
    [
        "add-annotation",
        "--kind=Exempt_On",
        "--justification",
        "unexpected C failure",
        "--location=13:4",
        f"--output={annotation_file}",
        "--annotation-id=exempt_on_1",
        "../main.adb",
    ]
    + gprsw.cov_switches,
    force_project_args=True,
)
xcov(
    [
        "add-annotation",
        "--kind=Exempt_Off",
        "--location=15:11",
        f"--external-annotations={annotation_file}",
        f"--output={annotation_file}",
        "--annotation-id=exempt_off_1",
        "../main.adb",
    ]
    + gprsw.cov_switches,
    force_project_args=True,
)

# Check we get the intended coverage result
cov_1 = "cov_1"
xcov(cov_args + [f"--output-dir={cov_1}"], out="cov_1.log")
check_xcov_reports(
    cov_1,
    expected_cov={
        "main.adb.xcov": {"+": {10, 11}, "*": {13, 14, 15}},
        "foo.c.xcov": {"+": {4}},
    },
)

show_log = "show.log"

# Show the annotations and check against a baseline
xcov(
    [
        "show-annotations",
        f"--external-annotations={annotation_file}",
    ]
    + gprsw.cov_switches,
    out=show_log,
)

thistest.fail_if_diff(
    baseline_file="../show_expected.txt",
    actual_file=show_log,
    failure_message='Unexpected "gnatcov show-annotations" output',
)

# Delete the annotations and add a new one
xcov(
    [
        "delete-annotation",
        f"--external-annotations={annotation_file}",
        "--annotation-id=exempt_on_1",
        f"--output={annotation_file}",
    ],
)
xcov(
    [
        "delete-annotation",
        f"--external-annotations={annotation_file}",
        "--annotation-id=exempt_off_1",
        f"--output={annotation_file}",
    ],
)
xcov(
    [
        "add-annotation",
        "--kind=Exempt_Region",
        "--justification",
        "Useless exemption",
        "--start-location=4:3",
        "--end-location=4:12",
        f"--output={annotation_file}",
        "--annotation-id=exempt_region_1",
        "../foo.c",
    ]
    + gprsw.cov_switches,
    force_project_args=True,
)

# Re-compute report to ensure the annotations have indeed been modified
# Check we get the intended coverage result
cov_2 = "cov_2"
xcov(cov_args + [f"--output-dir={cov_2}"])
check_xcov_reports(
    cov_2,
    expected_cov={
        "main.adb.xcov": {"+": {10, 11}, "!": {13}, "-": {14}},
        "foo.c.xcov": {"#": {4}},
    },
)

thistest.result()
