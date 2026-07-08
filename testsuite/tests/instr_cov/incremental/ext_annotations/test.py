"""
Simple sanity checking test for the add-annotation, delete-annotation and
show-annotation commands.
"""

from SCOV.minicheck import xcov, xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
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
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")

# Instrument without external annotations
xcov_instrument(gprsw, covlevel="stmt+decision")

# Add an annotation file
xcov(
    [
        "add-annotation",
        "--kind=Exempt_On",
        "--justification",
        "whatever",
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

# Instrument with external annotations. GNATcoverage should reinstrument
# every file.
xcov_instrument(
    gprsw,
    covlevel="stmt+decision",
    extra_args=[f"--external-annotations={annotation_file}"],
    out="instrument-with-switch.out",
)
thistest.fail_if_diff(
    baseline_file="../instrument-with-switch.expected",
    actual_file="instrument-with-switch.out",
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
        "whatever",
        "--start-location=4:1",
        "--end-location=4:12",
        f"--output={annotation_file}",
        "--annotation-id=exempt_region_1",
        "../foo.c",
    ]
    + gprsw.cov_switches,
    force_project_args=True,
)
# Check that gnatcov instrument reinstruments the file
xcov_instrument(
    gprsw,
    covlevel="stmt+decision",
    extra_args=[f"--external-annotations={annotation_file}"],
    out="instrument-with-annotations.out",
)
thistest.fail_if_diff(
    baseline_file="../instrument-with-annotations.expected",
    actual_file="instrument-with-annotations.out",
)
thistest.result()
