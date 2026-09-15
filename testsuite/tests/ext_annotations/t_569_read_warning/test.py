"""
Check that the warning displayed when the annotations file cannot be read is
formatted as expected.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import xcov


tmp = Wdir("tmp_")

xcov(
    [
        "show-annotations",
        "--external-annotations=nosuchfile.toml",
        "../main.adb",
    ],
    out="output.txt",
    tolerate_messages=".*",
)
thistest.fail_if_not_equal(
    '"gnatcov show-annotations" output',
    "warning: nosuchfile.toml: No such file or directory",
    contents_of("output.txt").strip(),
)

thistest.result()
