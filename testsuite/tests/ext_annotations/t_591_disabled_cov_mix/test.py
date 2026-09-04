"""
Check that mixing in-source and external annotation to create disable coverage
regions works as expected, and in particular that annotation fingerprints take
both into account (i.e. changing in-source annotations or external annotations
before re-instrumenting result in different annotation fingerprints, and thus
prevent source trace loading).
"""

import glob
import os.path

from e3.fs import cp, mkdir, mv

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

# Create the test project. Its source directory will be populated in the loop
# below.
p = gprfor(srcdirs=["."], mains=["main.adb"], langs=["Ada", "C"])
is_even_filename = os.path.abspath("is_even.c")
is_even_header_filename = os.path.abspath("is_even.h")
main_filename = os.path.abspath("main.adb")


# Instrument the test project and produce traces with various combinations of
# disabled coverage regions. Save both SID files and trace files aside for
# later re-use.
for label, with_in_source_annotations, with_external_annotations in [
    ("both", True, True),
    ("in_source", True, False),
    ("ext", False, True),
]:
    thistest.log(f"== Instrumentation for {label} ==")

    for src in ["main.adb", "is_even.c", "is_even.h"]:
        with open(os.path.join("..", src)) as f:
            lines = f.readlines()

        with open(src, "w") as f:
            for line in lines:
                if with_in_source_annotations or "REMOVEME" not in line:
                    print(line, file=f)
                else:
                    print("", file=f)

    xcov_args = build_and_run(
        gprsw=GPRswitches(root_project=p),
        mains=["main"],
        covlevel="stmt",
        extra_instr_args=(
            ["--external-annotations", "../annotations.toml"]
            if with_external_annotations
            else []
        ),
        extra_coverage_args=[],
    )
    mv(xcov_args[-1], f"t_{label}.srctrace")

    sids_dir = f"sids-{label}"
    mkdir(sids_dir)
    cp("obj/*.sid", sids_dir)


# Try to load source traces with mismatching SID files and ensure that the
# disabled coverage region inconsistencies are detected.
#
# Use SID files from the "both" instrumentation to load source traces from the
# other instrumentations.
sid_args = [
    f"--sid={filename}"
    for filename in glob.glob(os.path.join("sids-both", "*.sid"))
]
for label, src_trace in [
    ("in_source", "t_in_source.srctrace"),
    ("ext", "t_ext.srctrace"),
]:
    thistest.log(f"== [{label}] Loading source trace {src_trace} ==")

    log = f"coverage-{label}.txt"
    xcov(
        [
            "coverage",
            "-cstmt",
            "--save-checkpoint",
            f"cons_{label}.ckpt",
            src_trace,
            *sid_args,
        ],
        out=log,
        tolerate_messages=".*",
    )
    thistest.fail_if_not_equal(
        '"gnatcov coverage" output',
        f"warning: traces for {is_even_filename} (from {src_trace}) are"
        " inconsistent with the corresponding Source Instrumentation Data\n"
        f"warning: traces for {is_even_header_filename} (from {src_trace}) are"
        " inconsistent with the corresponding Source Instrumentation Data\n"
        f"warning: traces for {main_filename} (from {src_trace}) are"
        " inconsistent with the corresponding Source Instrumentation Data",
        "\n".join(sorted(lines_of(log))),
    )

thistest.result()
