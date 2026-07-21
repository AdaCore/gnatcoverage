"""
Check coverage reports with a branch exemption in the various part of an Ada
IF/ELSIF/ELSE block.
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
mains = [f"test_{i}.adb" for i in range(5)]
gpr = gprfor(srcdirs=[".."], mains=mains)
full_args = build_and_run(
    gprsw=GPRswitches(root_project=gpr, units=["pkg"]),
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
    ("0", {7, 11}, ["test_0.adb"], {"-": {6, 8, 9, 10, 13}, "*": {7, 11}}),
    (
        "1",
        {9, 13},
        ["test_1.adb"],
        {"-": {8, 10, 11}, "!": {6}, "+": {7}, "*": {9, 13}},
    ),
    (
        "23",
        {7},
        ["test_2.adb", "test_3.adb"],
        {"-": {13}, "!": {10}, "+": {8, 9, 11}, "*": {6, 7}},
    ),
    (
        "14",
        {11},
        ["test_1.adb", "test_4.adb"],
        {"-": {9}, "!": {8}, "+": {6, 7, 13}, "*": {10, 11}},
    ),
    (
        "1234",
        {9},
        ["test_1.adb", "test_2.adb", "test_3.adb", "test_4.adb"],
        {"#": {8, 9}, "+": {6, 7, 10, 11, 13}},
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
                "../pkg.adb",
            ]
        )

    output_dir = f"xcov-{label}"
    xcov(
        traceless_args
        + ["--output-dir", output_dir, "--external-annotations", annotations]
        + [main_to_trace[m] for m in mains],
        out=f"cov-{label}.log",
    )
    check_xcov_reports(
        output_dir,
        {"pkg.ads.xcov": {}, "pkg.adb.xcov": expected_pkg_cov},
    )

thistest.result()
