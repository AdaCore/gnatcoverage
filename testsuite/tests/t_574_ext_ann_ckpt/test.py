"""
Check that checkpoints include external annotations even when no coverage
report is produced.
"""

from __future__ import annotations

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    Exempt_Off,
    Exempt_On,
    generate_annotations,
    gprfor,
    xcov,
)


tmp = Wdir("tmp_")

annotation_file = generate_annotations(
    [
        Exempt_On(os.path.abspath("../main.adb"), "3:1", "3:1", "J"),
        Exempt_Off(os.path.abspath("../main.adb"), "3:80", "3:80"),
    ]
)
ckpt = "c.ckpt"
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[
        "--external-annotations",
        annotation_file,
        "--save-checkpoint",
        ckpt,
    ],
)
xcov(
    [
        "coverage",
        "--level=stmt",
        f"--checkpoint={ckpt}",
        "--annotate=xcov",
        "--output-dir=xcov",
    ]
)
# Previously, "gnatcov coverage" did not load external annotations when not
# generating a report: we were getting only "+" coverage here.
check_xcov_reports("xcov", {"main.adb.xcov": {"#": {3}}})

thistest.result()
