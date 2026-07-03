"""
Check the handling of the lifecycle of external annotations (add-annotation,
show-annotation, import) for fine grained exemptions.
"""

from e3.fs import cp

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

cp("../main.adb", "main.adb")
gpr = gprfor(srcdirs=["."], mains=["main.adb"])
filename = "annotations.toml"


def check_args(
    label: str,
    args: list[str],
    expected_error: str,
    expect_success: bool = False,
) -> None:
    thistest.log(f"== {label} ==")
    out = f"actual-{label}.txt"
    p = xcov(
        [
            "add-annotation",
            "--output",
            filename,
            *args,
            "main.adb",
        ],
        tolerate_messages=".*",
        register_failure=expect_success,
        out=out,
    )
    thistest.fail_if(
        not expect_success and p.status == 0,
        '"gnatcov add-annotation" failure expected',
    )
    thistest.fail_if_not_equal(
        '"gnatcov add-annotation" output',
        expected_error,
        contents_of(out).strip(),
    )


check_args(
    "args_missing_justification",
    ["--kind=Exempt_Decision_Outcome", "--location=7:7", "--outcome=false"],
    "warning: --justification missing for a --kind=Exempt_Decision_Outcome"
    " annotation",
    expect_success=True,
)
check_args(
    "args_missing_outcome",
    [
        "--kind=Exempt_Decision_Outcome",
        "--location=7:7",
        "--justification=Message",
    ],
    "gnatcov: --outcome missing for a --kind=Exempt_Decision_Outcome"
    " annotation",
)
check_args(
    "args_bad_outcome",
    [
        "--kind=Exempt_Decision_Outcome",
        "--location=7:7",
        "--justification=Message",
        "--outcome=notabool",
    ],
    "gnatcov: Invalid argument for --outcome: notabool",
)
check_args(
    "args_bad_decision",
    [
        "--kind=Exempt_Decision_Outcome",
        "--location=7:7",
        "--justification=Message",
        "--outcome=true",
        "--decision=A",
    ],
    "gnatcov: Invalid argument for --decision: A",
)


def check_validate(filename: str, expected_error: str) -> None:
    thistest.log(f"== validate: {filename} ==")
    out = f"actual-validate_{filename}.txt"
    xcov(
        [
            "show-annotations",
            f"--external-annotations=../{filename}",
            "main.adb",
        ],
        tolerate_messages=".*",
        out=out,
    )
    thistest.fail_if_not_equal(
        '"gnatcov show-annotation" output',
        expected_error,
        contents_of(out).strip(),
    )


def missing_or_invalid_msg(what: str) -> str:
    return (
        f"warning: Missing or invalid {what} for external exemption annotation"
        ' "foo", it will be ignored.'
    )


def invalid_msg(what: str) -> str:
    return (
        f'warning: Invalid {what} for external exemption annotation "foo", it'
        " will be ignored."
    )


check_validate(
    "err_decision_outcome_1.toml", missing_or_invalid_msg("outcome")
)
check_validate(
    "err_decision_outcome_2.toml", missing_or_invalid_msg("outcome")
)
check_validate("err_decision_outcome_3.toml", invalid_msg("decision offset"))
check_validate("err_decision_outcome_4.toml", invalid_msg("decision offset"))


thistest.log("== show_annotation ==")
out = "actual-show_annotation.txt"
xcov(
    [
        "show-annotations",
        "--external-annotations=../annotations.toml",
        "main.adb",
    ],
    out=out,
)
thistest.fail_if_diff("../show_baseline.txt", out)

thistest.result()
