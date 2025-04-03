"""
Test basic functionality of the buffer reset mechanism, in both C and Ada
source files.
"""

import glob
import re

from SCOV.minicheck import build_and_run, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    Dump_Buffers,
    generate_annotations,
    gprfor,
    Reset_Buffers,
)

annotations = generate_annotations(
    [
        Dump_Buffers("main_c.c", "9:3", None, trace_prefix='"c-0"'),
        Reset_Buffers("main_c.c", "10:3", None),
        Dump_Buffers("main_c.c", "12:3", None, trace_prefix='"c-1"'),
        Reset_Buffers("main_c.c", "13:3", None),
        Dump_Buffers("main_c.c", "15:3", None, trace_prefix='"c-2"'),
        Dump_Buffers("main_ada.adb", "8:5", None, trace_prefix='"ada-0"'),
        Reset_Buffers("main_ada.adb", "9:5", None, insert_after=True),
        Dump_Buffers("main_ada.adb", "11:5", None, trace_prefix='"ada-1"'),
        Reset_Buffers("main_ada.adb", "12:5", None, insert_after=True),
        Dump_Buffers("main_ada.adb", "14:5", None, trace_prefix='"ada-2"'),
    ]
)

tmp = Wdir("tmp_")

# Map from language to the corresponding main report name, and lines with
# coverage obligations.
main_lines = {
    "ada": ("main_ada.adb.xcov", [7, 10, 13]),
    "c": ("main_c.c.xcov", [8, 11, 14, 16]),
}

# Units which will only have a partial coverage result in them, regardless of
# the trace.
default_part_cov = {
    "ada": {"pkg.adb.xcov": {"!": {5}}, "pkg.ads.xcov": {}},
    "c": {"pkh.c.xcov": {"!": {6}}},
}

# Default "nothing executed" expected result for each unit
default_no_cov = {
    "ada": {
        "main_ada.adb.xcov": {"-": {7, 10, 13}},
        "pkg.adb.xcov": {"-": {5}},
        "pkg.ads.xcov": {},
    },
    "c": {
        "main_c.c.xcov": {"-": {8, 11, 14, 16}},
        "pkh.c.xcov": {"-": {6}},
    },
}


def get_expected_cov(trace_name):
    """
    Generate an expected coverage results based on the prefix of the trace
    indicating which main was executed, and its index.
    """
    match = re.match(pattern=r"(.*)-(\d)\.srctrace", string=trace_name)
    thistest.fail_if(
        not match, comment="trace name not in expected format: " + trace_name
    )
    trace_lang = match.group(1)
    idx = int(match.group(2))
    expected_cov = {}

    lines = main_lines[trace_lang][1]
    for lang in ("ada", "c"):
        if lang == trace_lang:
            expected_cov.update(
                {
                    main_lines[lang][0]: {
                        "+": {lines[idx]},
                        "-": {lines[i] for i in range(len(lines)) if i != idx},
                    }
                }
            )
            expected_cov.update(default_part_cov[lang])
        else:
            expected_cov.update(default_no_cov[lang])
    return expected_cov


def check_one_exec(cov_args, lang):
    """
    Find the traces generated by the lang executable, assuming the trace prefix
    is lang. Then create a coverage report from each trace, checking the
    expected coverage report.
    """

    thistest.log(f"======== Checking {lang} traces =========")

    # We expect the trace to be in the format
    # {lang}-{index}.srctrace:
    traces = glob.glob(f"{lang}-[0-9].srctrace")

    # There is three dump indications in each main, we should thus have the
    # same number of traces.
    thistest.fail_if(
        len(traces) != 3, comment=f"expected 3 traces, found {len(traces)}"
    )
    traces.sort()

    for i in range(len(traces)):
        thistest.log(f"-- {i + 1}. --")
        output_dir = f"output_{lang}_{i}/"
        xcov(
            cov_args + [f"--output-dir={output_dir}", traces[i]],
            out=f"coverage_{lang}_{i}.log",
        )
        check_xcov_reports(output_dir, get_expected_cov(traces[i]))


prj_id = "p"

# Instrument build and run
cov_args = build_and_run(
    gprsw=GPRswitches(
        gprfor(
            prjid=prj_id,
            srcdirs=[".."],
            mains=["main_ada.adb", "main_c.c"],
        )
    ),
    covlevel="stmt+mcdc",
    mains=["main_ada", "main_c"],
    dump_trigger="manual",
    manual_prj_name=prj_id,
    extra_coverage_args=["-axcov+"],
    extra_instr_args=[
        "--dump-filename-simple",
        f"--external-annotations={annotations}",
    ],
)

# Check the individual trace contents for each executable
for lang in ["c", "ada"]:
    check_one_exec(cov_args, lang)

# Do a consolidated check with all the traces. We expect exactly six traces and
# no coverage violations except for the final return statement in the c main.
thistest.log("========== global consolidated check =========")
all_traces = glob.glob("*.srctrace")
thistest.fail_if(
    len(all_traces) != 6, comment=f"expected 6 traces, got {len(all_traces)}"
)
output_dir = "consolidated/"
xcov(
    cov_args + [f"--output-dir={output_dir}"] + all_traces,
    out="consolidated.log",
)
check_xcov_reports(
    output_dir,
    {
        "main_ada.adb.xcov": {"+": {7, 10, 13}},
        "pkg.ads.xcov": {},
        "pkg.adb.xcov": {"+": {5}},
        "main_c.c.xcov": {"+": {8, 11, 14}, "-": {16}},
        "pkh.c.xcov": {"+": {6}},
    },
)

thistest.result()
