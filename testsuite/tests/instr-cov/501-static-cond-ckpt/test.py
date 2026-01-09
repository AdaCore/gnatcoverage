"""
Regression testcase: checkpoint loading code used to mix SCO_Id indexes from
the checkpoint and indexes from gnatcov's in-memory tables, resulting in
various inconsistencies/crashes.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


level = "stmt+mcdc"
checkpoints = []

# Produce one checkpoint with different staticness for conditions in pkg.adb.
# It is important for the first checkpoint to have "main" as a unit of interest
# but not the second checkpoint: this makes SCOs in the two checkpoints
# different, and allows to trigger the "inappropriate SCO index usage".
for i, units in [
    (0, ["main", "pkg"]),
    (1, ["pkg"]),
]:
    tmp = Wdir(f"tmp_{i}")

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(
                mains=["main.adb"],
                srcdirs=[".."],
                extra=f"""
                    package Naming is
                        for Spec ("params") use "params__{i}.ads";
                    end Naming;
                """,
            ),
            units=units,
        ),
        covlevel=level,
        mains=["main"],
        extra_coverage_args=["--save-checkpoint=c.ckpt"],
        trace_mode="src",
    )
    checkpoints.append(os.path.abspath("c.ckpt"))

    tmp.to_homedir()

tmp = Wdir("tmp_")

# The index confusions (and crashes) happened when processing statically
# evaluated decisions during checkpoint loading (more precisely: when loading
# their SCIs).
xcov(
    [
        "coverage",
        f"--level={level}",
        "--annotate=xcov",
        "--output-dir=xcov",
        f"--checkpoint={checkpoints[0]}",
        f"--checkpoint={checkpoints[1]}",
    ]
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {7, 8, 9}},
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {13, 14, 16}},
    },
)

thistest.result()
