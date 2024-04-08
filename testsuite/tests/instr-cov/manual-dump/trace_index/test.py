"""
Test that when multiple traces are dumped by the same executable, no trace file
gets overwritten and we get as many traces as there are dump indications in
the sources.
"""

import glob

from SCOV.minicheck import build_and_run, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, srctrace_pattern_for

tmp = Wdir("tmp_")

expected_cov = [
    {"+": {5}, "-": {7, 9}},
    {"+": {5, 7}, "-": {9}},
    {"+": {5, 7, 9}},
]


def check_trace(cov_args, traces, index):
    """
    Create a coverage report from cov_args and traces[index], in the
    report_{index} directory, and check the report against expected_cov,
    for the corresponding index.
    """
    thistest.log(f"======= Checking trace {index + 1} =========")
    output_dir = f"report_{index + 1}"
    xcov(
        cov_args + [traces[index], f"--output-dir={output_dir}"],
        out=f"xcov_{index}",
    )
    check_xcov_reports(output_dir, {"main.adb.xcov": expected_cov[index]})


cov_args = build_and_run(
    gprsw=GPRswitches(
        gprfor(
            prjid="p",
            srcdirs=[".."],
            mains=["main.adb"],
        )
    ),
    covlevel="stmt",
    mains=["main"],
    dump_trigger="manual",
    manual_prj_name="p",
    extra_coverage_args=["-axcov"],
)

# Remove the traces from the coverage arguments
cov_args = cov_args[:-3]

traces = glob.glob(
    srctrace_pattern_for("main", manual=True, manual_prj_name="p")
)
traces.sort()

thistest.fail_if(
    len(traces) != 3, comment=f"Expected three traces, got {len(traces)}"
)

# Check that the traces do indeed contain an index in their filename
thistest.fail_if(
    any(not traces[i].endswith(f"-{i}.srctrace") for i in range(3)),
    comment=f"missing trace index in trace filenames: {traces}",
)

# Check for each trace the coverage results
for i in range(3):
    check_trace(cov_args, traces, i)

thistest.result()
