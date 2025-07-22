"""
Check that gnatcov produces expected assembly coverage reports for single-byte
instructions covered by a single-byte trace entry.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tracelib import (
    TraceEntry,
    TraceFile,
    TraceOp,
    TraceKind,
    create_exec_infos,
    create_trace_header,
)
from SUITE.tutils import (
    do,
    exepath_to,
    gprbuild,
    gprfor,
    tracename_for,
    xcov,
)


wd = Wdir("tmp_")
main = exepath_to("main")
main_trace = tracename_for("main")

# Build the 'main' executable
gpr = gprfor(["main.c"], srcdirs="..", langs=("C", "Asm"))
gprbuild(gpr)

# Get the address of the 'f' symbol in it
for line in do(["nm", main]).splitlines():
    parts = line.split()
    name = parts[-1]
    if name == "f":
        f_addr = int(parts[0], 16)
        break
else:
    thistest.failed("Cannot find symbol 'f' in executable 'main'")

# Create a dummy binary trace from it, including a trace block that covers only
# the 1-byte trailing "ret" instruction.
EM_X86_64 = 62
bits = 64
pc_size = bits // 8
tf = TraceFile(
    first_header=create_trace_header(
        TraceKind.Info, pc_size, big_endian=False, machine=EM_X86_64
    ),
    infos=create_exec_infos(main),
    second_header=create_trace_header(
        TraceKind.Flat, pc_size, big_endian=False, machine=EM_X86_64
    ),
    entries=[
        TraceEntry(bits, f_addr + start_off, end_off - start_off, op)
        for start_off, end_off, op in [
            (0x0, 0x4, TraceOp.Block | TraceOp.Br1),
            (0x4, 0x1C, TraceOp.Block | TraceOp.Br1),
            (0x1C, 0x1E, TraceOp.Block),
            (0x20, 0x21, TraceOp.Block),
        ]
    ],
)
with open(main_trace, "wb") as f:
    tf.write(f)

# Make sure we get the expected coverage report
xcov(
    ["coverage", "-cbranch", "-aasm", main_trace, "--routines=f"],
    out="coverage.log",
)
actual_report = contents_of("coverage.log").splitlines()


def fmt(offset):
    return hex(f_addr + offset)[2:].rjust(16, "0")


expected_report = f"""\
Coverage level: branch
f !: {fmt(0)}-{fmt(0x20)}
{fmt(0x00)} +:  85 ff            test   %edi,%edi
{fmt(0x02)} v:  7e 1a            jle    {hex(f_addr + 0x1e)} <f+0x1e>
{fmt(0x04)} +:  31 c9            xor    %ecx,%ecx
{fmt(0x06)} +:  31 c0            xor    %eax,%eax
{fmt(0x08)} +:  0f 1f 84 00 00 00 00 00   nopl   0x0(%rax,%rax,1)
{fmt(0x10)} +:  89 ca            mov    %ecx,%edx
{fmt(0x12)} +:  21 fa            and    %edi,%edx
{fmt(0x14)} +:  01 d0            add    %edx,%eax
{fmt(0x16)} +:  ff c1            inc    %ecx
{fmt(0x18)} +:  39 cf            cmp    %ecx,%edi
{fmt(0x1a)} v:  75 f4            jne    {hex(f_addr + 0x10)} <f+0x10>
{fmt(0x1c)} +:  eb 02            jmp    {hex(f_addr + 0x20)} <f+0x20>
{fmt(0x1e)} -:  31 c0            xor    %eax,%eax
{fmt(0x20)} +:  c3               ret
14 instructions analyzed:
  13 covered
  1 not executed
2 conditional branches analyzed:
  0 fully covered
  2 partially covered
  0 not executed
"""
with open("expected.txt", "w") as f:
    f.write(expected_report)

thistest.fail_if_diff(
    baseline_file="expected.txt",
    actual_file="coverage.log",
    failure_message="unexpected coverage report",
)

thistest.result()
