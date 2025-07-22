"""
Test that the instrumentation of C sources with filenames that contain uncommon
characters produces valid instrumented sources.
"""

import os.path

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.control import env
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Copy the sources in the temporary directory. Note that we cannot test the
# case of a filename containing a double quote or a backslash on Windows
# because of filename restrictions on that platform.
copy_map = {
    "ada_main.adb": "ada_main.adb",
    "bar.c": "src bar.c" if env.build.os.name == "windows" else 'src\\"bar.c',
    "foo.c": "src foo$@.c",
}
for src, dest in copy_map.items():
    cp(os.path.join("..", src), dest)

# Compute the expected coverage report from the actual source filenames. Note
# that in xcov filenames, "gnatcov coverage" first turns '\' to '/' (during
# path separator canonicalization) and then the unique filename machinery turns
# '/' to '-'.
coverage_data = {
    "ada_main.adb": {"+": {10, 12}},
    "bar.c": {"+": {4, 5}, "-": {7}},
    "foo.c": {"+": {4}},
}
expected_report = {
    "{}.xcov".format(copy_map[filename].replace("\\", "-")): report
    for filename, report in coverage_data.items()
}

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=["."], mains=["ada_main.adb"], langs=["Ada", "C"]
        )
    ),
    covlevel="stmt",
    mains=["ada_main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports("xcov", expected_report)

thistest.result()
