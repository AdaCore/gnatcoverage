"""
Check that "gnatcov extract-base64-trace" can extract trace data from outputs
with arbitrary line prefixes.
"""

import itertools

from SCOV.instr import xcov_convert_base64, xcov_instrument
from SCOV.minicheck import check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import exepath_to, gprbuild, gprfor, run_cov_program, xcov

tmp = Wdir("tmp_")

# Instrument and build the test project
p = GPRswitches(
    root_project=gprfor(srcdirs="..", mains=["main1.adb", "main2.adb"]),
    units=["pkg"],
)
xcov_instrument(
    gprsw=p,
    covlevel="stmt",
    dump_trigger="main-end",
    dump_channel="base64-stdout",
)
gprbuild(p.root_project, trace_mode="src")

# Run the two programs, saving their output
mains = ["main1", "main2"]
out_files = [f"{m}_output.txt" for m in mains]

for main, out_file in zip(mains, out_files):
    run_cov_program(exepath_to(main), out=out_file)

# Expected coverage report for both program executions:
main1_cov = {"pkg.adb.xcov": {"+": {11, 14}, "-": {12}}, "pkg.ads.xcov": {}}
main2_cov = {"pkg.adb.xcov": {"+": {11, 12}, "-": {14}}, "pkg.ads.xcov": {}}

# Now combine them with different prefixes and check that "gnatcov
# extract-base64-trace" correctly loads the last one.
for label, interleaved_outputs, trailing_output, cov in [
    ("main1_first", out_files, None, main1_cov),
    ("main2_first", [out_files[1], out_files[0]], None, main2_cov),
    ("main2_trail", [out_files[0]], out_files[1], main2_cov),
]:
    thistest.log(f"== {label} ==")

    tracedata_filename = f"{label}_trace_data.txt"
    trace_filename = f"{label}.srctrace"

    traces_lines = []
    for i, filename in enumerate(interleaved_outputs):
        with open(filename) as f:
            traces_lines.append((f"DATA{i}> ", f.readlines()))

    if trailing_output is None:
        trailing_lines = []
    else:
        with open(trailing_output) as f:
            trailing_lines = f.readlines()

    # Interleave all lines to make sure that once gnatcov detects a source
    # trace with some prefix, it filters that prefix only until the end of the
    # source trace.
    with open(tracedata_filename, "w") as f:
        for i in itertools.count(0):
            had_line = False
            for prefix, lines in traces_lines:
                if i >= len(lines):
                    continue
                had_line = True
                f.write(prefix)
                f.write(lines[i])
            if not had_line:
                break

        for line in trailing_lines:
            f.write("TRAILING> ")
            f.write(line)

    # Now run the source trace conversion and create a coverage report from it
    xcov_convert_base64(tracedata_filename, trace_filename)
    xcov_dir = f"xcov-{label}"
    xcov(
        [
            "coverage",
            "--level=stmt",
            "--annotate=xcov",
            "--output-dir",
            xcov_dir,
            trace_filename,
        ]
        + p.cov_switches
    )
    check_xcov_reports(xcov_dir, cov, discard_empty=False)

thistest.result()
