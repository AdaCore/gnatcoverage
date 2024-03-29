#! /usr/bin/env python
# -*- coding: utf-8 -*-

import collections
import re
import subprocess

import intervalmap


CODE_SYMBOL_LINE = re.compile(
    b"^(?P<pc>[0-9a-f]+) (?P<size>[0-9a-f]+)"
    b" [Tt] (?P<name>[a-zA-Z0-9_$@.]+)$"
)
Symbol = collections.namedtuple("Symbol", "pc size name")


def format_symbol(symbol):
    return "{} ({:x}-{:x})".format(
        symbol.name, symbol.pc, symbol.pc + symbol.size
    )


def debug_interval(intval):
    seen_bounds = set()
    for i, bound in enumerate(intval.bounds):
        try:
            value = intval.values[bound]
        except KeyError:
            suffix = ""
        else:
            suffix = "-> {:x}".format(intval.bounds[i + 1]), value
        print("{:02} - {:x}{}".format(i, bound, suffix))
        seen_bounds.add(bound)
    print("---")
    for unseen in set(intval.values.keys()) - seen_bounds:
        print("-> {:x}".format(unseen), intval.values[unseen])
    print("---")


def get_sym_info(exe_filename):
    """Parse symbol info in `exe_filename` and return it as an interval map,
    and the symbols could not be inserted in the interval map because of
    address overlapping.

    The result maps from program counter to a symbol.
    """
    sym_info = intervalmap.IntervalMap()
    overlap_syms = []

    # Let nm parse ELF and the symbol table for us.
    proc = subprocess.Popen(["nm", "-S", exe_filename], stdout=subprocess.PIPE)

    n = 0
    while True:
        n += 1
        # Read as many lines as possible from nm.
        line = proc.stdout.readline().decode("ascii")
        if not line:
            break

        # Process symbols defined in the text (code) section only.
        m = CODE_SYMBOL_LINE.match(line)
        if not m:
            continue

        pc = int(m.group("pc"), 16)
        size = int(m.group("size"), 16)
        symbol = Symbol(pc, size, m.group("name"))
        try:
            sym_info[pc : pc + size] = symbol
        except ValueError:
            overlap_syms.append(symbol)

    return sym_info, overlap_syms


if __name__ == "__main__":
    import sys

    binary = sys.argv[1]
    address = int(sys.argv[2], 16)

    sym_info, overlap_syms = get_sym_info(binary)
    if overlap_syms:
        sys.stderr.write("warning: some symbols overlap with others:\n")
        for sym in overlap_syms:
            sys.stderr.write("  - {}\n".format(sym_info.format_symbol(sym)))
    try:
        symbol = sym_info[address]
    except KeyError:
        print("Not found")
        sys.exit(1)
    else:
        print(
            "{:#08x} is inside {}:"
            " {:#08x}-{:#08x}"
            " ({:#x}/{:#x} bytes)".format(
                address,
                symbol.name,
                symbol.pc,
                symbol.pc + symbol.size,
                address - symbol.pc,
                symbol.size,
            )
        )
