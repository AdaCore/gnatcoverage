"""
Check that command line switches that accept response files (like --units) are
correctly interpreted.
"""

import os

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


Wdir("tmp_")

# Instrument, build and run the example project
xcov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["test_foobar.adb"], srcdirs="../src")
    ),
    covlevel="stmt",
    mains=["test_foobar"],
    extra_coverage_args=["--annotate=xcov"],
)

for label, units, expected_xcov in [
    ("default", None, {"bar.adb", "bar.ads", "foo.adb", "test_foobar.adb"}),
    ("bar-only", ["bar"], {"bar.adb", "bar.ads"}),
    ("foo-only", ["foo"], {"foo.adb"}),
    ("non-test", ["bar", "foo"], {"bar.adb", "bar.ads", "foo.adb"}),
]:
    thistest.log(f"== {label} ==")

    xcov_dir = f"xcov-{label}"
    extra_args = [f"--output-dir={xcov_dir}"]
    if units:
        resp_file = f"resp-{label}.txt"
        with open(resp_file, "w") as f:
            for u in units:
                print(u, file=f)
        extra_args.append(f"--units=@{resp_file}")

    # Generate a report, with the list of requested units passed to --units
    # through a response file, then check that we get the expected list of
    # files in the XCOV coverage report.
    xcov(xcov_args + extra_args)
    thistest.fail_if_not_equal(
        "list of XCOV reports",
        "\n".join(sorted([f"{f}.xcov" for f in expected_xcov])),
        "\n".join(
            sorted([f for f in os.listdir(xcov_dir) if f.endswith(".xcov")])
        ),
    )

thistest.result()
