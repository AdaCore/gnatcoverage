"""
Check that "gnatcov add-annotation" emits the expected error message when
adding a duplicate exemption region.
"""

import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import xcov


main_adb = os.path.abspath("main.adb")
tmp = Wdir("tmp_")
log = "output.txt"
filename = "annotations.toml"

# Create a first annotation
xcov(
    [
        "add-annotation",
        "--output",
        filename,
        "--annotation-id=A1",
        "--kind=Exempt_On",
        "--location=4:1",
        "--justification=J1",
        main_adb,
    ]
)

# Try to add a second annotation at the same location
xcov(
    [
        "add-annotation",
        "--external-annotations",
        filename,
        "--output",
        filename,
        "--annotation-id=A2",
        "--kind=Exempt_On",
        "--location=4:1",
        "--justification=J2",
        main_adb,
    ],
    out=log,
    tolerate_messages=".*",
)
thistest.fail_if_not_equal(
    "'gnatcov add-annotation' output",
    "warning: Pre-existing annotation with identifier A1 matches the same"
    " region.",
    contents_of(log).strip(),
)

thistest.result()
