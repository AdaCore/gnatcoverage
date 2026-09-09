"""
Regression test: check that file patterns in external annotations (here:
`tmp_/main.adb` are matched against fully resolved filenames
(`/path/to/testcase/working/dir/tmp_/main.adb`) rather than the filename given
to `gnatcov show-annotations` (`main.adb`).
"""

import os.path

from e3.fs import cp, mkdir

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov


Wdir("tmp_")

cp("../main.adb", ".")
p = gprfor(srcdirs=["."], mains=["main.adb"])
mkdir("obj")

expected = f"""
{os.path.abspath("main.adb")}:
- 3:4 - 3:8; id: region; kind: Exempt_Region; Justification: dummy exemption
"""

xcov(
    [
        "show-annotations",
        "-P",
        p,
        "--external-annotations",
        "../annotations.toml",
        "main.adb",
    ],
    out="gnatcov.out",
)

thistest.fail_if_not_equal(
    '"gnatcov show-annotations" output', expected, contents_of("gnatcov.out")
)

thistest.result()
