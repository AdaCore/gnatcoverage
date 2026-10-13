"""
Check that "gnatcov add-annotation" synthetizes distinct annotation IDs for
fine grained exemptions that target the same source locations but with
different details.
"""

import os.path

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


main_adb = os.path.abspath("main.adb")

tmp = Wdir("tmp_")

# Instrument the project only once
gpr = gprfor(srcdirs=[".."], mains=["main.adb"])
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    trace_mode="src",
)

# Then try to produce one coverage report per couple of external annotations
for label, common_args, extra_args1, extra_args2 in [
    (
        "dc_out__out",
        ["--kind=Exempt_Decision_Outcome", "--location=10:1"],
        ["--outcome=False"],
        ["--outcome=True"],
    ),
    (
        "dc_out__dc",
        [
            "--kind=Exempt_Decision_Outcome",
            "--location=10:1",
            "--outcome=True",
        ],
        ["--decision=0"],
        ["--decision=1"],
    ),
    (
        "dc_cond__cond",
        ["--kind=Exempt_Decision_Condition", "--location=10:1"],
        ["--condition=1"],
        ["--condition=2"],
    ),
    (
        "dc_cond__dc",
        [
            "--kind=Exempt_Decision_Condition",
            "--location=10:1",
            "--condition=1",
        ],
        ["--decision=0"],
        ["--decision=1"],
    ),
    (
        "full_dc",
        ["--kind=Exempt_Full_Decision", "--location=10:1"],
        ["--decision=0"],
        ["--decision=1"],
    ),
    (
        "man_dc_e__values",
        ["--kind=Manual_Decision_Evaluation", "--location=10:1"],
        ["--values=TT"],
        ["--values=TF"],
    ),
    (
        "man_dc_e__dc",
        [
            "--kind=Manual_Decision_Evaluation",
            "--location=10:1",
            "--values=TT",
        ],
        ["--decision=0"],
        ["--decision=1"],
    ),
]:
    thistest.log(f"== {label} ==")

    annot_filename = f"a_{label}.toml"
    annot_args = ["--external-annotations", annot_filename]
    output_dir = f"xcov_{label}"

    # Generate external annotations from the two sets of arguments
    with open(annot_filename, "w"):
        pass
    for extra_args, justification in [
        (extra_args1, "J1"),
        (extra_args2, "J2"),
    ]:
        xcov(
            [
                "add-annotation",
                *annot_args,
                "--output",
                annot_filename,
                "--justification",
                justification,
                main_adb,
                *common_args,
                *extra_args,
            ]
        )

    # Now compute code coverage using these annotations and check the report
    xcov(xcov_args + annot_args + ["--output-dir", output_dir])
    check_xcov_reports(
        output_dir,
        {
            "main.adb.xcov": {"+": {7, 8, 11}, "!": {10}},
            "func_helpers.adb.xcov": {"+": {4}},
            "func_helpers.ads.xcov": {},
        },
    )

thistest.result()
