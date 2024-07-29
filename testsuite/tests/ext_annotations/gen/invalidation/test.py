"""
Basic test concerning invalidation of annotations by gnatcov when the
annotated sources have been modified beyond what stable_sloc can manage.
"""

import os

from SCOV.minicheck import (
    build_run_and_coverage,
    check_xcov_reports,
    contents_of,
    xcov,
)
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

annotation_file = "annotations.toml"

prefix = os.path.join("..", "v1", "")

# Create annotations for the V1 sources
xcov(
    [
        "add-annotation",
        "--kind=Exempt_Region",
        "--justification",
        "Useless code",
        "--start-location=4:7",
        "--end-location=4:16",
        f"--output={annotation_file}",
        "--annotation-id=exempt_ada",
        f"--source-root={prefix}",
        "../v1/main.adb",
    ]
)
xcov(
    [
        "add-annotation",
        "--kind=Exempt_Region",
        "--justification",
        "Useless code",
        "--start-location=5:3",
        "--end-location=5:12",
        f"--external-annotations={annotation_file}",
        f"--output={annotation_file}",
        "--annotation-id=exempt_c",
        f"--source-root={prefix}",
        "../v1/main_c.c",
    ]
)

# Create a project from V2 sources
gprsw = GPRswitches(
    root_project=gprfor(
        mains=["main.adb"],
        srcdirs=["../v2"],
        langs=["Ada", "C"],
    )
)

# Try to show the annotations on the modified sources
show_log = "show.log"
xcov(
    [
        "show-annotations",
        f"--external-annotations={annotation_file}",
        "--quiet",
    ]
    + gprsw.cov_switches,
    out=show_log,
)

thistest.fail_if_diff(
    baseline_file="../show_expected.txt",
    actual_file=show_log,
    failure_message='Unexpected "gnatcov show-annotations" output',
)

# Run the coverage workflow, check for expected warnings and that no
# annotations have been taken into acount. Do not consider main_c.c as
# a main to avoid issues with the last_chance_chandler, the coverage results
# are secondary anyways.
coverage_log = "coverage.log"
build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[
        "-axcov",
        f"--external-annotations={annotation_file}",
    ],
    tolerate_coverage_messages="Stale annotation for main.*",
    out=coverage_log,
)

thistest.fail_if_no_match(
    "Unexpected 'gnatcov coverage' warnings:",
    regexp=r"(\n?warning: Stale annotation for main.*){2}",
    actual=contents_of(coverage_log),
)

# We expect violations and no exempted regions
check_xcov_reports(
    reports_dir="obj",
    expected_cov={
        "main.adb.xcov": {"+": {6, 9, 12}},
        "main_c.c.xcov": {"-": {6, 12}},
    },
)

thistest.result()
