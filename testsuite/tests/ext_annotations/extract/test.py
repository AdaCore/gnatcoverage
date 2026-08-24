"""
Check the "gnatcov extract-annotations" command.

It turns the in-source annotations of a project (pragma Annotate (Xcov, ...)
for Ada, GNATCOV_* comments for C) into equivalent external annotations. With
-i, it also removes them from the sources, in which case the generated
annotations must designate the sources as rewritten: check that coverage is
then unchanged.
"""

import os
import os.path
import shutil

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


SRC = os.path.abspath("src")


def setup(prjid: str) -> GPRswitches:
    """
    Populate the current directory with a fresh copy of the sources and a
    project for them. Return the corresponding GPRswitches.
    """
    shutil.copytree(SRC, "src")
    return GPRswitches(
        root_project=gprfor(
            mains=["test_extract.adb"],
            srcdirs=["src"],
            langs=["Ada", "C", "C++"],
            prjid=prjid,
            objdir="obj",
        )
    )


# Coverage expectations with the annotations still in the sources. Lines 6 and
# 11 of pkg.adb hold the Exempt_On / Exempt_Off pragmas, lines 17 and 20 the
# Cov_Off / Cov_On ones, lines 6 and 9 of helper.c and lines 7 and 15 of
# tricky.cpp the GNATCOV_EXEMPT_* comments: an annotation is part of the region
# it delimits, so its own line is covered by that region too.
IN_SOURCE_COV = {
    "pkg.adb.xcov": {
        "+": {5, 12},
        "*": {6, 7, 8, 9, 10, 11},
        "D": {17, 18, 19, 20},
    },
    "pkg.ads.xcov": {},
    "helper.c.xcov": {"+": {10}, "*": {6, 7, 8, 9}},
    "tricky.cpp.xcov": {"*": {7, 8, 9, 10, 11, 12, 13, 14, 15}},
    "test_extract.adb.xcov": {"+": {10, 11, 12}},
}

# Same, once the annotations have been removed: each of them took a whole line,
# so everything below them shifts up, and the regions now start and end on the
# code they used to surround.
MIGRATED_COV = {
    "pkg.adb.xcov": {"+": {5, 10}, "*": {6, 7, 8, 9}, "D": {15, 16}},
    "pkg.ads.xcov": {},
    "helper.c.xcov": {"+": {8}, "*": {6, 7}},
    "tricky.cpp.xcov": {"*": {7, 8, 9, 10, 11, 12, 13}},
    "test_extract.adb.xcov": {"+": {10, 11, 12}},
}

tmp = Wdir("tmp_")

# 1. Baseline: the in-source annotations, as they are

wd = Wdir("in_source")
gprsw = setup("in_source")
build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["test_extract"],
    extra_coverage_args=["-axcov", "--output-dir=cov"],
)
check_xcov_reports("cov", IN_SOURCE_COV, discard_empty=False)
wd.to_homedir()

# 2. Extraction alone must not touch the sources, and must produce one
#    Exempt_Region per Exempt_On/Exempt_Off pair plus the Cov_Off/Cov_On pair.

wd = Wdir("extract_only")
gprsw = setup("extract_only")
xcov(
    [
        "extract-annotations",
        f"-P{gprsw.root_project}",
        "--output=annotations.toml",
    ],
    out="extract.log",
    tolerate_messages=(
        r"Could not create an auto-relocating annotation for .*tricky\.cpp"
    ),
)
annotations = contents_of("annotations.toml")

for purpose, count in [
    ("xcov.exempt.region", 3),
    ("xcov.cov.off", 1),
    ("xcov.cov.on", 1),
]:
    thistest.fail_if_not_equal(
        f'number of "{purpose}" annotations extracted',
        count,
        annotations.count(f'purpose = "{purpose}"'),
    )

thistest.fail_if(
    'justification = "defensive code"' not in annotations,
    "the Exempt_On justification was not carried over to the annotation file",
)
thistest.fail_if(
    'justification = "not tested yet"' not in annotations,
    "the Cov_Off justification was not carried over to the annotation file",
)
thistest.fail_if(
    'justification = "unreachable in practice"' not in annotations,
    "the C exemption justification was not carried over to the annotation"
    " file",
)

# The self-relocating backends must be preferred over absolute locations
for kind, count in [
    ('kind = "lal_context"', 3),
    ('kind = "clang_context"', 1),
]:
    thistest.fail_if_not_equal(
        f"number of {kind} matchers", count, annotations.count(kind)
    )

thistest.fail_if_no_match(
    "Exempt_On pragma left in pkg.adb",
    r"(?s).*pragma Annotate \(Xcov, Exempt_On.*",
    contents_of(os.path.join("src", "pkg.adb")),
)
wd.to_homedir()

# 3. Migration: -i removes the annotations from the sources, and the generated
#    annotations must yield the very same coverage as the in-source ones.

wd = Wdir("in_place")
gprsw = setup("in_place")
xcov(
    [
        "extract-annotations",
        f"-P{gprsw.root_project}",
        "--output=annotations.toml",
        "-i",
    ],
    out="extract.log",
)

pkg_adb = contents_of(os.path.join("src", "pkg.adb"))
helper_c = contents_of(os.path.join("src", "helper.c"))
for what, text, pattern in [
    ("pkg.adb", pkg_adb, "pragma Annotate"),
    ("helper.c", helper_c, "GNATCOV_"),
    ("tricky.cpp", contents_of(os.path.join("src", "tricky.cpp")), "GNATCOV_"),
]:
    thistest.fail_if(
        pattern in text,
        f"{pattern} still present in {what} after extract-annotations -i",
    )

# The rewriting must not leave blank lines behind
thistest.fail_if_no_match(
    "unexpected pkg.adb contents after migration",
    r"(?s).*Ok := False;\n      if X < 0 then.*",
    pkg_adb,
)

thistest.fail_if(
    'R"(a " b /* not a comment */)"'
    not in contents_of(os.path.join("src", "tricky.cpp")),
    "the C++ raw string literal did not survive extract-annotations -i",
)

build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["test_extract"],
    extra_coverage_args=[
        "-axcov",
        "--output-dir=cov",
        "--external-annotations=annotations.toml",
    ],
    extra_instr_args=["--external-annotations=annotations.toml"],
)
check_xcov_reports("cov", MIGRATED_COV, discard_empty=False)
wd.to_homedir()

thistest.result()
