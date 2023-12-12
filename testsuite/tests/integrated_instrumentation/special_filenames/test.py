"""
Check that the integration instrumentation handles correctly filenames that
contain spaces.
"""

import os
import os.path

from e3.fs import cp

from SUITE.control import env
from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov

Wdir("tmp_")

# Copy the sources in the temporary directory. Note that we cannot test the
# case of a filename containing a double quote or a backslash on Windows
# because of filename restrictions on that platform.
copy_map = {
    "bar.c": 'src bar.c' if env.build.os.name == "windows" else 'src\\"bar.c',
    "foo.c": "src foo$@.c",
    "test.c": "test.c",
}
for src, dest in copy_map.items():
    cp(os.path.join("..", src), dest)

# Compute the expected coverage report from the actual source filenames. Note
# that in xcov filenames, "gnatcov coverage" first turns '\' to '/' (during
# path separator canonicalization) and then the unique filename machinery turns
# '/' to '-'.
coverage_data = {
    "test.c": {"+": {7, 8, 9}},
    "foo.c": {"+": {4, 7}, "-": {5}},
    "bar.c": {"+": {4}},
}
expected_report = {
    "{}.xcov".format(copy_map[filename].replace("\\", "-")): report
    for filename, report in coverage_data.items()
}

# Setup the instrumentation process
sources = [os.path.abspath(filename) for filename in copy_map.values()]
files = [f"--files={filename}" for filename in sources]
xcov(
    ["setup-integration", "--level=stmt", "--compilers=gcc", "--output-dir=."]
    + files
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=os.getcwd())

# Build the test program and run it
cmdrun(["gcc", "-o", "test program"] + sources, for_pgm=False)
cmdrun(["test program"], for_pgm=False)

# Check coverage expectations
sid_args = [f"--sid={filename}.sid" for filename in sources]
xcov(
    ["coverage", "-cstmt", "-axcov", srctracename_for("test")]
    + sid_args
)
check_xcov_reports("*.xcov", expected_report)

thistest.result()
