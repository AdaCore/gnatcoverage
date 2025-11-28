"""
Check that the special --print-gpr-registry command line switch works as
expected.

Specifically, run "gnatcov --print-gpr-registry" with the 4 possible format
options and check that they have roughly the same content (same summary: list
of attributes).
"""

import json

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import xcov


tmp = Wdir("tmp_")


def summarize_json(f):
    result = []
    doc = json.load(f)
    for pkg in doc["packages"]:
        for attr in pkg["attributes"]:
            result.append(f"{pkg['package_name']}.{attr['attribute_name']}")
    result.sort()
    return "\n".join(result)


def summarize_text(f):
    result = []
    for line in f:
        line = line.strip()
        if not line:
            continue
        elif " " not in line:
            pkg = line
        elif (
            line.endswith(": list")
            or line.endswith(": single")
            or line.endswith(": list, indexed")
            or line.endswith(": single, indexed")
        ):
            attr = line.split()[0][:-1]
            result.append(f"{pkg}.{attr}")
    result.sort()
    return "\n".join(result)


baseline = (
    "Coverage.Excluded_Routines"
    "\nCoverage.Excluded_Routines_List"
    "\nCoverage.Excluded_Source_Files"
    "\nCoverage.Excluded_Source_Files_List"
    "\nCoverage.Excluded_Units"
    "\nCoverage.Excluded_Units_List"
    "\nCoverage.Routines"
    "\nCoverage.Routines_List"
    "\nCoverage.Switches"
    "\nCoverage.Units"
    "\nCoverage.Units_List"
)


for fmt, summarize in [
    (None, summarize_json),
    ("text", summarize_text),
    ("json", summarize_json),
    ("json-compact", summarize_json),
]:
    thistest.log(f"== {fmt} ==")

    args = ["--print-gpr-registry"]
    if fmt is None:
        out_filename = "default.txt"
    else:
        args.append(f"--gpr-registry-format={fmt}")
        out_filename = f"{fmt}.txt"

    xcov(args, out=out_filename, auto_target_args=False)

    with open(out_filename) as f:
        actual = summarize(f)
    thistest.fail_if_not_equal("Output of " + " ".join(args), baseline, actual)


thistest.log("== invalid ==")
args = ["--print-gpr-registry", "--gpr-registry-format=invalid"]
p = xcov(
    args, out="invalid.txt", auto_target_args=False, register_failure=False
)
thistest.fail_if_not_equal("gnatcov exit code", 1, p.status)
thistest.fail_if_no_match(
    "Output of " + " ".join(args),
    "^.*gnatcov.*: Bad GPR registry format: invalid$",
    contents_of("invalid.txt").strip(),
)


thistest.result()
