"""
Check that "gnatcov instrument" correctly reports progress about the
instrumented units.
"""

import dataclasses

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            langs=["Ada", "C", "C++"],
            mains=["main.adb"],
            srcdirs=[".."],
        ),
        units=["main", "c_unit.c", "cpp_unit.cpp"],
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    quiet=False,
    trace_mode="src",
)

# Sanity check: the insrument-build-coverage process completed with the
# expected results.
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {7, 8}},
        "c_unit.c.xcov": {"+": {7, 8}},
        "cpp_unit.cpp.xcov": {"+": {10}},
    },
)


# Units are not instrumented in a particular order: we only want to check that
# all of them are listed with the expected formatting.
@dataclasses.dataclass
class Section:
    label: str
    lines: list[str]


sections = [Section("<pre-section>", [])]
for line in lines_of("instrument.log"):
    if line.startswith(" "):
        sections[-1].lines.append(line)
    else:
        sections.append(Section(line, []))

sorted_lines = []
for section in sections:
    sorted_lines.append(section.label)
    sorted_lines += sorted(section.lines)

thistest.fail_if_not_equal(
    "'gnatcov instrument' output",
    "\n".join(
        [
            "<pre-section>",
            "Coverage instrumentation",
            "   [Ada]           main",
            "   [C++]           cpp_unit.cpp",
            "   [C]             c_unit.c",
            "Main instrumentation",
            "   [Ada]           main",
        ]
    ),
    "\n".join(sorted_lines),
)

thistest.result()
