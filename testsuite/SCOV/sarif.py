"""
Helpers to check SARIF reports.
"""

import json
import os.path

from SUITE.context import thistest


def check_sarif_report(baseline_filename: str, actual_filename: str) -> None:
    """
    Check that some actual SARIF report matches the corresponding baseline.
    """

    def refine(v):
        match v:
            case dict():
                if "uri" in v:
                    v["uri"] = os.path.basename(v["uri"])
                for item in v.values():
                    refine(item)

            case list():
                for item in v:
                    refine(item)

    # Rewrite the actual report so that computing a diff is possible (e.g.
    # basenames instead of URIs) and is human friendly (indentation).
    modified_actual = f"modified-{os.path.basename(actual_filename)}"
    with open(actual_filename) as f:
        doc = json.load(f)
    refine(doc)
    for run in doc["runs"]:
        driver = run["tool"]["driver"]
        if driver["name"] == "gnatcov":
            driver["version"] = "<version number>"
    with open(modified_actual, "w") as f:
        json.dump(doc, f, indent=2)
        f.write("\n")

    thistest.fail_if_diff(
        baseline_filename,
        modified_actual,
        failure_message=f"unexpected content for {actual_filename}",
        ignore_white_chars=False,
    )
