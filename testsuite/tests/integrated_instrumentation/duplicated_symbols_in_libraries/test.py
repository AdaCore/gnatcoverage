"""
Check that gnatcov integrated instrumentation only includes coverage buffer
symbols that are in the main link closure. It used to include in the coverage
buffers list every coverage buffer symbol that was in a library / object file
on the link command line. This could result in pulling into the link closure
object files that would have not been included otherwise, and thus result in
clashing symbols.

Also check that gnatcov generates unique SID names when there are homonym
files
"""

import os
import os.path

from e3.fs import cp

from SCOV.minicheck import check_xcov_reports
from SUITE.control import env
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov

Wdir("tmp_")

cwd = os.getcwd()

# Copy the sources and the Makefile in the temporary directory
for item in ["Makefile", "libbar", "libfoo", "main.c"]:
    cp(os.path.join("..", item), ".", recursive=True)

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join(cwd, 'libbar', 'bar.c')}",
        f"--files={os.path.join(cwd, 'libbar', 'foo.c')}",
        f"--files={os.path.join(cwd, 'libfoo', 'bar.c')}",
        f"--files={os.path.join(cwd, 'libfoo', 'foo.c')}",
        "--compilers=gcc",
        f"--output-dir={cwd}",
        "-v",
        "--save-temps",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Then, run the build process unchanged
cmdrun(["make"], for_pgm=False)

# Run the executable
cmdrun(["main"], for_pgm=True)

# Check coverage expectations
log_file = "coverage.log"
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=libbar-foo.c.sid",
        "--sid=libbar-bar.c.sid",
        "--sid=libfoo-foo.c.sid",
        "--sid=libfoo-bar.c.sid",
        "-axcov",
        srctracename_for("main"),
    ],
    out=log_file,
)

# TODO: the warnings regarding homonym filenames are unexpected but they do not
# affect the testcase.
expected_warning = ("Warning: same base name for files:"
    '\r?\n  {libfoo_foo}'
    '\r?\n  {libbar_foo}'
    "\r?\nWarning: same base name for files:"
    '\r?\n  {libfoo_bar}'
    '\r?\n  {libbar_bar}').format(
        libfoo_foo=os.path.join(cwd, "libfoo", "foo.c"),
        libbar_foo=os.path.join(cwd, "libbar", "foo.c"),
        libfoo_bar=os.path.join(cwd, "libfoo", "bar.c"),
        libbar_bar=os.path.join(cwd, "libbar", "bar.c"),
    )

thistest.fail_if_no_match(
    '"gnatcov output" ({})'.format(log_file),
    expected_warning,
    contents_of(log_file),
)
check_xcov_reports(
    ".",
    {
        "libbar-bar.c.xcov": {"+": {4}},
        "libbar-foo.c.xcov": {"+": {4}},
        "libfoo-bar.c.xcov": {"-": {4}},
        "libfoo-foo.c.xcov": {"-": {4}},
    },
)

thistest.result()
