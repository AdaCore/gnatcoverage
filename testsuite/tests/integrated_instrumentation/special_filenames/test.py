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
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    LinkMain,
    CompileSource,
)
from SUITE.tutils import thistest

Wdir("tmp_")

# Copy the sources in the temporary directory. Note that we cannot test the
# case of a filename containing a double quote or a backslash on Windows
# because of filename restrictions on that platform.
copy_map = {
    "bar.c": "src bar.c" if env.build.os.name == "windows" else 'src\\"bar.c',
    "foo.c": "src foo$@.c",
    "main.c": "main.c",
}
for src, dest in copy_map.items():
    cp(os.path.join("..", src), dest)

# Compute canonicalized filenames, which will be the base names for gnatcov
# artifacts (SID and xcov files). Note that "gnatcov" first turns '\' to '/'
# (during path separator canonicalization) and then the unique filename
# machinery turns '/' to '-'.
canonicalized_filenames = {
    filename: mapped.replace("\\", "-")
    for filename, mapped in copy_map.items()
}

# Compute the expected coverage report from the actual source filenames
coverage_data = {
    "main.c": {"+": {7, 8, 9}},
    "foo.c": {"+": {4, 7}, "-": {5}},
    "bar.c": {"+": {4}},
}
expected_report = {
    "{}.xcov".format(canonicalized_filenames[filename]): report
    for filename, report in coverage_data.items()
}

# Initialize the workflows
sources = [os.path.abspath(filename) for filename in copy_map.values()]
wfs = []
objects = []
for source in sources:
    c = CompileSource(source=source)
    wfs.append(c)
    objects.append(c.out)
wfs.append(LinkMain(objects=objects))

build_run_and_coverage(wfs, files_of_interest=sources)
check_xcov_reports(".", expected_report)

thistest.result()
