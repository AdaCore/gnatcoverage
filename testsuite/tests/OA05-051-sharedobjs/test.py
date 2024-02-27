"""
This peculiar testcase attemps to test shared objects support in source
coverage in "gnatcov" without the corresponding handling in trace producers.

In order to do this, it crafts multiple "artificial" trace files with various
trace entries and checks that "gnatcov coverage" produces the reports we expect
for each trace file.
"""

import os.path
import struct
import subprocess

from e3.fs import mkdir, rm

from SUITE.cutils import lines_of
from SUITE.tutils import gprbuild, thistest, xcov

from SUITE.tracelib import (
    TraceEntry,
    TraceSpecial,
    TraceFile,
    TraceOp,
    TraceKind,
    create_exec_infos,
    create_trace_header,
)


# Clean artifaacts from previous runs
if os.path.exists("tmp"):
    rm("tmp", recursive=True)
mkdir("tmp")


def in_tmp(*elts):
    return os.path.join("tmp", *elts)


# Build the program along with the shared libraries. Since we build with shared
# libraries, set LIBRARY_TYPE accordingly so that the implict with of
# gnatcov_rts_full works fine.
gprbuild("myprog", gargs=["-XLIBRARY_TYPE=relocatable"])
myprog_filename = in_tmp("bin", "myprog")


# And now create a set of traces we'll use to test "gnatcov coverage"


class Symbol(object):
    def __init__(self, addr, size):
        self.addr = addr
        self.size = size

    @property
    def first(self):
        return self.addr

    @property
    def last(self):
        return self.addr + self.size - 1


def get_symbol(filename, symbol):
    for line in (
        subprocess.check_output(["nm", "-S", filename])
        .decode("utf-8")
        .splitlines()
    ):
        if line.endswith(symbol):
            addr, size, _ = line.split(None, 2)
            return Symbol(int(addr, 16), int(size, 16))
    raise ValueError("Could not find symbol {} in {}".format(symbol, filename))


ELFHeader = struct.Struct("<16s" "H" "H")  # e_ident  # e_type  # e_machine
EI_CLASS = 4
ELFCLASS32 = 1
ELFCLASS64 = 2

EI_DATA = 5
ELFDATA2LSB = 1
ELFDATA2MSB = 2

# Get architecture properties from the ELF we just built
with open(myprog_filename, "rb") as f:
    hdr = ELFHeader.unpack(f.read(ELFHeader.size))
    e_ident = hdr[0]
    e_type = hdr[1]
    e_machine = hdr[2]

    bits = {ELFCLASS32: 32, ELFCLASS64: 64}[e_ident[EI_CLASS]]
    big_endian = e_ident[EI_DATA] == ELFDATA2MSB
    machine = (
        ((e_machine & 0xFF) << 8) | (e_machine >> 8)
        if big_endian
        else e_machine
    )

so_load_addr = 0xCCFF0000

trace_file_info = create_exec_infos(myprog_filename)

libnames = ("mylib1", "mylib2")
libfiles = {
    libname: in_tmp("lib", "lib{}.so".format(libname)) for libname in libnames
}
syms = {
    libname: get_symbol(libfile, "{}__f".format(libname))
    for libname, libfile in libfiles.items()
}

load_events = {
    libname: TraceEntry(
        bits,
        so_load_addr,
        TraceSpecial.LoadSharedObject,
        TraceOp.Special,
        create_exec_infos(libfiles[libname], code_size=sym.last + 1),
    )
    for libname, sym in syms.items()
}
unload_event = TraceEntry(
    bits, so_load_addr, TraceSpecial.UnloadSharedObject, TraceOp.Special
)


def write_traces(trace_filename, executed_libs):
    entries = []
    for libname in libnames:
        entries.append(load_events[libname])
        if libname in executed_libs:
            sym = syms[libname]
            entries.append(
                TraceEntry(
                    bits, so_load_addr + sym.addr, sym.size, TraceOp.Block
                )
            )
        entries.append(unload_event)

    tf = TraceFile(
        create_trace_header(TraceKind.Info, bits // 8, big_endian, machine),
        trace_file_info,
        create_trace_header(TraceKind.Flat, bits // 8, big_endian, machine),
        entries,
    )

    with open(trace_filename, "wb") as f:
        tf.write(f)


for trace_filename, executed_libs in [
    ("myprog_0.trace", []),
    ("myprog_1.trace", ["mylib1"]),
    ("myprog_2.trace", ["mylib2"]),
    ("myprog_12.trace", ["mylib1", "mylib2"]),
]:
    write_traces(in_tmp(trace_filename), executed_libs)


# Traces are ready: let's see how coverage goes!


def cov_report_summary(output_dir):
    summary = []
    for cov_file in os.listdir(output_dir):
        cov_set = set()
        cov_filepath = os.path.join(output_dir, cov_file)
        assert cov_file.endswith(".xcov")
        cov_file = cov_file[:-5]
        for line in lines_of(cov_filepath):
            try:
                prefix, _ = line.split(":", 1)
                _, prefix = prefix.rsplit(" ", 1)
            except ValueError:
                continue
            if prefix in "+-":
                cov_set.add(prefix)
        summary.append((cov_file, "".join(sorted(cov_set))))
    return sorted(summary)


def format_summary(summary, indent=""):
    return "\n".join(
        "{}{}: {}".format(indent, cov_file, cov_set)
        for cov_file, cov_set in summary
    )


expectations = {
    "0": [("mylib1.adb", "-"), ("mylib2.adb", "-"), ("myprog.adb", "-")],
    "1": [("mylib1.adb", "+"), ("mylib2.adb", "-"), ("myprog.adb", "-")],
    "2": [("mylib1.adb", "-"), ("mylib2.adb", "+"), ("myprog.adb", "-")],
    "12": [("mylib1.adb", "+"), ("mylib2.adb", "+"), ("myprog.adb", "-")],
}

for tag, exp in expectations.items():
    output_dir = in_tmp("report-{}".format(tag))
    if os.path.exists(in_tmp(output_dir)):
        rm(output_dir, recursive=True)
    mkdir(output_dir)

    xcov(
        [
            "coverage",
            "-Pmyprog",
            "--level=stmt",
            "--annotate=xcov",
            "--output-dir",
            output_dir,
            in_tmp("myprog_{}.trace".format(tag)),
        ]
    )
    summary = cov_report_summary(output_dir)
    thistest.fail_if(
        summary != exp,
        "Expected the following coverage report:\n"
        "{}\n"
        "But got the following one instead:\n"
        "{}".format(format_summary(exp, "  "), format_summary(summary, "  ")),
    )

thistest.result()
