"""
Check that gnatcov reinstruments all sources when --force is passed.
"""

from e3.fs import mkdir

from SCOV.minicheck import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Create the test project
gprsw = GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"]))
mkdir("obj")


log_lines = {
    "main.adb": "[Ada Instrument]  main.adb",
    "pkg.adb": "[Ada Instrument]  pkg.adb",
}


def check(
    label: str,
    extra_args: list[str],
    expected_sources: set[str],
) -> None:
    """
    Run "gnatcov instrument" and check the set of instrumented files.
    """
    thistest.log(f"== {label} ==")

    out = f"instr-{label}.txt"
    xcov_instrument(
        gprsw=gprsw,
        covlevel="stmt",
        extra_args=extra_args,
        quiet=False,
        out=out,
    )
    thistest.fail_if_not_equal(
        '"gnatcov instrument" output',
        "\n".join(sorted(log_lines[f] for f in expected_sources)),
        "\n".join(sorted(lines_of(out))),
    )


all_sources = {"main.adb", "pkg.adb"}

# Instrument a first time
check("default-1", [], all_sources)

# Sanity check: instrument a second time, with no change
check("default-2", [], set())

# Force instrumentation (no parallelism)
check("force-serial", ["--force"], all_sources)

# Force instrumentation (forced parallelism)
check("force-parallel", ["--force", "--force-parallelism"], all_sources)

thistest.result()
