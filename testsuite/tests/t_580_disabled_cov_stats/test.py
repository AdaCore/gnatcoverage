"""
Check that lines in a disabled coverage region are not accounted for in the
line coverage ratio: such lines carry no coverage obligation, so a unit whose
code is entirely disabled must be reported as containing no code, just like the
obligation stats already do.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

prj = gprfor(srcdirs=["../src"], mains=["main.adb"])

build_run_and_coverage(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-a", "xcov", "-a", "xml"],
)


def stats_lines(filename: str) -> list[str]:
    """
    Return the two summary lines of the given "xcov" report: the line stats
    first, then the obligation stats.
    """
    return contents_of(filename).splitlines()[1:3]


def metric_tags(filename: str) -> list[str]:
    """
    Return the "total_lines_of_relevance" and "disabled_coverage" <metric> tags
    of the first statistics block in the given XML report.
    """
    return [
        line.strip()
        for line in contents_of(filename).splitlines()
        if "total_lines_of_relevance" in line or "disabled_coverage" in line
    ][:2]


# dis.adb holds nothing but a disabled coverage region: both summary lines must
# agree that there is no code to report.

thistest.fail_if_not_equal(
    "dis.adb summary",
    ["no code", "no code"],
    stats_lines("obj/xcov/dis.adb.xcov"),
)

# pkg.adb holds two coverable lines, both covered, plus a disabled region: the
# disabled lines must be left out of the ratio.

thistest.fail_if_not_equal(
    "pkg.adb summary",
    ["100% of 2 lines covered", "100% statement coverage (2 out of 2)"],
    stats_lines("obj/xcov/pkg.adb.xcov"),
)

# Likewise for the XML report: disabled lines are not part of the total, so
# they must not be presented as a ratio of it (pkg.adb has more disabled lines
# than lines of relevance).

thistest.fail_if_not_equal(
    "dis.adb.xml metrics",
    [
        '<metric kind="total_lines_of_relevance" count="0"/>',
        '<metric kind="disabled_coverage" count="5"/>',
    ],
    metric_tags("obj/xml/dis.adb.xml"),
)

thistest.fail_if_not_equal(
    "pkg.adb.xml metrics",
    [
        '<metric kind="total_lines_of_relevance" count="2"/>',
        '<metric kind="disabled_coverage" count="5"/>',
    ],
    metric_tags("obj/xml/pkg.adb.xml"),
)

thistest.result()
