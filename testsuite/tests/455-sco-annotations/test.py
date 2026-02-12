"""
Check that it is possible to re-use traces when only Ada annotations were added
to source file between two coverage analysis.
"""

import os.path

from e3.fs import cp, mkdir, mv

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of, list_to_file
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


# List of annotations that we plan to add to "main.adb"
annotations = [
    (23, 4, 'xcov, Exempt_On, "test code"'),
    (23, 4, 'gnatcheck, Exempt_On, "no_exception", "test code"'),
    (24, 4, 'gnatcheck, Exempt_Off, "no_exception", "test code"'),
    (24, 4, "xcov, Exempt_Off"),
    (27, 1, 'xcov, Exempt_On, "test code"'),
    (28, 2, 'gnatcheck, Exempt_On, "no_exception", "test code"'),
    (29, 2, 'gnatcheck, Exempt_Off, "no_exception", "test code"'),
    (30, 1, "xcov, Exempt_Off"),
]
# List of comments that we plan to add to "foo.c"
comments = [
    (4, 1, "First coverage unrelated comment"),
    (5, 2, 'GNATCOV_EXEMPT_ON "this is fine"'),
    (6, 2, "GNATCOV_EXEMPT_OFF"),
    (8, 1, "Second coverage unrelated comment"),
]


tmp = Wdir("tmp_")


mkdir("src")
for filename in ["main.adb", "foo.c"]:
    cp(f"../{filename}", "src")
gprsw = GPRswitches(gprfor(srcdirs=["src"], mains=["main.adb"]))

xcov_args = build_and_run(
    gprsw=gprsw,
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
)

# Put the trace aside
trace_file = xcov_args.pop()
saved_trace_file = "saved" + os.path.splitext(trace_file)[1]
mv(trace_file, saved_trace_file)
xcov_args.append(saved_trace_file)

# Sanity check: coverage report for original sources is as expected
xcov(xcov_args)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {
            "+": {5, 10, 11, 13, 14, 16, 18, 19, 20},
            "-": {23, 28},
            "!": {27},
        },
        "foo.c.xcov": {"+": {7}, "-": {5}, "!": {4}},
    },
)

# Add annotations. Process them in reverse order so that insertion by index
# works naturally.
lines = lines_of("src/main.adb")
for line_no, indent, args in reversed(annotations):
    lines.insert(line_no - 1, f"{'   ' * indent}pragma Annotate ({args});")
list_to_file(lines, "src/main.adb")

lines = lines_of("src/foo.c")
for line_no, indent, label in reversed(comments):
    lines.insert(line_no - 1, f"{'  ' * indent}// {label}")
list_to_file(lines, "src/foo.c")

# Reinstrument/compile
build_and_run(
    gprsw=gprsw,
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=[],
)

# Generate a coverage report from the new SID/ALI files but with the old trace
# file.
xcov(xcov_args)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {
            "+": {5, 10, 11, 13, 14, 16, 18, 19, 20},
            "*": {23, 24, 25, 26, 27, 31, 32, 33, 34, 35, 36, 37},
        },
        # C coverage with binary traces does not support in-sources exemptions
        "foo.c.xcov": (
            {"+": {10}, "*": {6, 7, 8}, "!": {5}}
            if thistest.options.trace_mode == "src"
            else {"+": {10}, "-": {7}, "!": {5}}
        ),
    },
)

thistest.result()
