"""
Check that gnatcov produces correct SCOs (i.e. with no source location nesting)
when there are redundant line markers resulting in nested SCOs.

This can happen with macro expansions mixing user-code and systems-header code
(such as the assert macro). The resulting expansion will have multiple line
markers, with a special flag for the code that comes from system headers.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


print(thistest.options)


def iter_versions():
    """Iterator on C versions supported by the current C toolchain."""
    tags = thistest.options.tags
    yield None
    yield "c99"
    if "5.04a1" in tags:
        return
    yield "c11"
    if "7.1.2" in tags:
        return
    yield "c17"
    if "morello-elf" in tags:
        return
    yield "c23"


# How the "assert" macro is expanded depends on the C standard version: test a
# representative set of them.
for std in iter_versions():
    thistest.log(f"== std: {std} ==")
    wd = Wdir(f"tmp_{std or 'default'}")

    # Build and produce a coverage report for the test project
    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(
                srcdirs=[".."],
                mains=["main.c"],
                compiler_extra=(
                    f"""
                    for Default_Switches ("C") use ("-std={std}");
                """
                    if std
                    else ""
                ),
            )
        ),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["-axcov", "--output-dir=xcov"],
        trace_mode="src",
    )
    check_xcov_reports("xcov", {"main.c.xcov": {"+": {6, 10, 14}}})

    wd.to_homedir()

thistest.result()
