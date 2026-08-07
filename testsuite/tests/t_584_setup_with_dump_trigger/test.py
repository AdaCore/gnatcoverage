"""
Check that passing `--dump-trigger` to gnatcov setup works as expected.
"""

import json

from SCOV.minicheck import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import xcov

AUTO_TRIGGERS = ["main-end", "atexit", "ravenscar-task-termination"]

for trigger in AUTO_TRIGGERS + ["manual"]:
    tmp = Wdir(f"tmp_{trigger}")
    xcov(
        ["setup", "-q", "--prefix=.", f"--dump-trigger={trigger}"],
        tolerate_messages="--dump-trigger=ravenscar-task-termination "
        "may not be compatible with the selected runtime",
    )

    # Check JSON manifest
    manifest = json.loads(
        contents_of("share/gnatcov_rts/setup-config-gnatcov_rts.json")
    )
    if trigger == "manual":
        thistest.fail_if(
            not manifest["manual-dump-trigger"],
            "manual-dump-trigger should be true",
        )
        thistest.fail_if(
            manifest.get("auto-dump-trigger") is not None,
            "manual-dump-trigger should be true",
        )
    else:
        thistest.fail_if(
            manifest["manual-dump-trigger"],
            "manual-dump-trigger should be false",
        )
        auto_trg = manifest.get("auto-dump-trigger")
        thistest.fail_if(
            auto_trg != trigger, f"expected {trigger}, got {auto_trg}"
        )

    tmp.to_homedir()

# Check one case of multi-dump trigger
Wdir("tmp_multiple")
xcov(
    [
        "setup",
        "-q",
        "--prefix=.",
        "--dump-trigger=atexit",
        "--dump-trigger=manual",
    ],
)
manifest = json.loads(
    contents_of("share/gnatcov_rts/setup-config-gnatcov_rts.json")
)
thistest.fail_if(
    not manifest["manual-dump-trigger"],
    "manual-dump-trigger should be true",
)
auto_trg = manifest.get("auto-dump-trigger")
thistest.fail_if(auto_trg != "atexit", f"expected atexit, got {auto_trg}")


thistest.result()
