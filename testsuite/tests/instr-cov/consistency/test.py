"""
Test that gnatcov properly report source trace files that are inconsistent with
the provided SID files.
"""

import os
import os.path

from e3.fs import cp, mkdir

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, xcov


p_gpr = "p.gpr"
first_sid_file = "first.sid"

tmp = Wdir("tmp_")

# As this test modifies source files, create a working copy
for filename in ("p.gpr", "main.adb"):
    cp(os.path.join("..", filename), filename)


# Instrument the input project and back up the SID file for main.adb
xcov_instrument(
    gprsw=GPRswitches(root_project=p_gpr), gpr_obj_dir="obj", covlevel="stmt"
)
cp(os.path.join("obj", "main.sid"), first_sid_file)

# Modify the source file and re-instrument it
with open("main.adb", "w") as f:
    f.write(
        """
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
    function Fact (I : Integer) return Integer is
    begin
        if I < 2 then
            return 1;
        else
            return I * Fact (I - 2);
        end if;
    end Fact;

begin
    Put_Line ("Fact (6) = " & Integer'Image (Fact (6)));
end Main;
"""
    )

# Now, instrument and build it. Then run the main program, letting it produce a
# trace.
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p_gpr),
    covlevel="stmt",
    gpr_obj_dir="obj",
    gpr_exe_dir="obj",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
srctrace = xcov_args[-1]

# Now try to run a coverage analysis with the trace file produced for the
# second SID file but giving gnatcov the first one.
out_file = "gnatcov.out"
mkdir("xcov")
xcov(
    [
        "coverage",
        "-axcov",
        "-cstmt",
        "--output-dir=xcov",
        "--sid",
        first_sid_file,
        srctrace,
    ],
    out=out_file,
    tolerate_messages=(
        r"traces for body of main \(from .*\) are"
        r" inconsistent with the corresponding Source Instrumentation Data"
    ),
)

# Check that gnatcov warns about inconsistent fingerprints
actual = contents_of(out_file).strip()
expected = (
    "warning: traces for body of main (from {}) are"
    " inconsistent with the corresponding Source Instrumentation Data".format(
        srctrace
    )
)
thistest.fail_if(
    expected != actual,
    'Unexpected output for "gnatcov coverage". Expected:\n'
    "{}\n"
    "but got:\n"
    "{}".format(indent(expected), indent(actual)),
)

# gnatcov only see the first SID file, so it is expected to have the coverage
# obligations coming from the first instrumentation. Besides, since it
# discarded info from the trace file, gnatcov is supposed to report violations
# for all coverage obligations related to the first version of "main.adb".
check_xcov_reports("xcov", {"main.adb.xcov": {"-": {5}}})

thistest.result()
