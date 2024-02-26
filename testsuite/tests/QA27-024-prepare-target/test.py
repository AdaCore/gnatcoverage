import re

from e3.os.process import Run

from SUITE.stream_decoder import ByteStreamDecoder
from SUITE.tracelib import TraceFile, InfoKind

from SUITE.cutils import Wdir
from SUITE.context import env
from SUITE.tutils import exepath_to, gprbuild, gprfor, thistest, tracename_for


tmp_ = Wdir("tmp_")

# We first build a dummy executable and execute
#
#   gnatcov run --target=prepare <exe>
#
# This is supposed to produce a trace file with only the trace info section
# (header and info entries).

gpr = gprfor(srcdirs=[".."], mains=["foo.adb"])

gprbuild(gpr)

exefile = exepath_to("foo")

# We run a straigtht "gnatcov run" of our own, bypassing the 'xcov'
# function circuitry to prevent the addition of a real --target in
# cross configurations.

bits = env.target.cpu.bits
cmd = ["gnatcov", "run", f"--target=prepare{bits}", exepath_to("foo")]
p = Run(cmd)
thistest.fail_if(
    p.status != 0,
    "command '%s' failed ; output was:\n%s" % (" ".join(cmd), p.out),
)

tracefile = tracename_for("foo")

with open(tracefile, "rb") as fp:
    trace = TraceFile.read(ByteStreamDecoder(fp))

# Check that the trace file contains the info section and at least some of the
# bits we expect there

thistest.fail_if(not trace.first_header, "info header missing from trace file")

thistest.fail_if(not trace.infos, "info entries missing from trace file")

trace_info = trace.infos.infos

# Entry kinds we expect to find in the info section header, and a regexp that
# the corresponding value should match. We're not trying to match a specific
# date (e.g. today()) in the exec time stamp to prevent getting into timezone
# intricacies.

expected_entries = {
    InfoKind.ExecFileName: re.escape(exefile.encode("utf-8")),
    InfoKind.ExecTimeStamp: (
        r"\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d".encode("ascii")
    ),
}

found_entries = {}

for info_entry in trace_info.values():
    thistest.fail_if(
        info_entry.kind in found_entries,
        "info entry kind %d found more than once in trace" % info_entry.kind,
    )

    if info_entry.kind in expected_entries:
        expected_contents = expected_entries[info_entry.kind]
        thistest.fail_if(
            not re.match(pattern=expected_contents, string=info_entry.data),
            "expected contents '%s' not found in entry of kind %d;\n"
            "value was '%s'"
            % (expected_contents, info_entry.kind, info_entry.data),
        )
        found_entries[info_entry.kind] = info_entry

thistest.fail_if(
    len(found_entries) != len(expected_entries),
    "trace info section missing expected contents",
)

# Check that the trace file does not contain the execution section.

thistest.fail_if(
    trace.second_header, "exec section header found in trace file, unexpected"
)

thistest.result()
