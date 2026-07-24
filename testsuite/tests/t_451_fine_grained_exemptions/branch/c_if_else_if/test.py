"""
Check coverage reports with a branch exemption in a complex IF/ELSE C setting.
"""

import os.path

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

# Instrument the project and run each main, then collect individually each
# trace corresponding to each main.
mains = [f"test_{i}.c" for i in range(5)]
gpr = gprfor(srcdirs=[".."], mains=mains)
full_args = build_and_run(
    gprsw=GPRswitches(root_project=gpr, units=["ops.c"]),
    covlevel="stmt+decision",
    mains=[os.path.splitext(m)[0] for m in mains],
    extra_coverage_args=["--annotate=xcov"],
)
traceless_args = full_args[: -len(mains)]
traces = sorted(full_args[-len(mains) :])
main_to_trace = dict(zip(mains, traces))

# Now compute coverage reports with various combinations of traces and branch
# exemptions.
for label, exempt_lines, mains, expected_pkg_cov in [
    ("0", {12, 18}, ["test_0.c"], {"-": {11, 13, 15, 17, 21}, "*": {12, 18}}),
    (
        "1",
        {14, 21},
        ["test_1.c"],
        {
            "-": {13, 17, 18},
            "!": {11},
            "+": {12},
            "*": {14, 15, 16, 20, 21, 22},
        },
    ),
    (
        "23",
        {12},
        ["test_2.c", "test_3.c"],
        {"-": {21}, "!": {17}, "+": {13, 15, 18}, "*": {11, 12}},
    ),
    (
        "14",
        {18},
        ["test_1.c", "test_4.c"],
        {"-": {15}, "!": {13}, "+": {11, 12, 21}, "*": {17, 18}},
    ),
    (
        "1234",
        {21},
        ["test_1.c", "test_2.c", "test_3.c", "test_4.c"],
        {"#": {17, 20, 21, 22}, "+": {11, 12, 13, 15, 18}},
    ),
]:
    thistest.log(f"== {label} ==")

    annotations = f"ann-{label}.toml"
    with open(annotations, "w"):
        pass
    for line in exempt_lines:
        xcov(
            [
                "add-annotation",
                "-P",
                gpr,
                "--output",
                annotations,
                "--external-annotations",
                annotations,
                "--kind=Exempt_Branch",
                f"--location={line}:1",
                f"--justification=Exemption #{line}",
                "../ops.c",
            ]
        )

    output_dir = f"xcov-{label}"
    xcov(
        traceless_args
        + ["--output-dir", output_dir, "--external-annotations", annotations]
        + [main_to_trace[m] for m in mains],
        out=f"cov-{label}.log",
    )
    check_xcov_reports(output_dir, {"ops.c.xcov": expected_pkg_cov})

thistest.result()
