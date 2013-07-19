# -*- coding: utf-8 -*-

import collections
import re
import subprocess

import intervalmap


CODE_SYMBOL_LINE = re.compile(
    b'^(?P<pc>[0-9a-f]+) (?P<size>[0-9a-f]+)'
    b' [Tt] (?P<name>[a-zA-Z0-9_$@.]+)$'
)
Symbol = collections.namedtuple('Symbol', 'pc size name')


def format_symbol(symbol):
    return '{} ({:x}-{:x})'.format(
        symbol.name,
        symbol.pc, symbol.pc + symbol.size
    )

def debug_interval(intval):
    seen_bounds = set()
    for i, bound in enumerate(intval.bounds):
        print '{:02} - {:x}'.format(i, bound),
        try:
            value = intval.values[bound]
        except KeyError:
            print ''
        else:
            print '-> {:x}'.format(intval.bounds[i + 1]), value
        seen_bounds.add(bound)
    print '---'
    for unseen in set(intval.values.keys()) - seen_bounds:
        print '-> {:x}'.format(unseen), intval.values[unseen]
    print '---'

def get_sym_info(exe_filename):
    """Parse symbol info in `exe_filename` and return it as an interval map.

    The result maps from program counter to a symbol.
    """
    sym_info = intervalmap.IntervalMap()

    # Let nm parse ELF and the symbol table for us.
    proc = subprocess.Popen(
        ['nm', '-S', exe_filename],
        stdout=subprocess.PIPE
    )

    n = 0
    while True:
        n += 1
        # Read as many lines as possible from nm.
        line = proc.stdout.readline().decode('ascii')
        if not line:
            break

        # Process symbols defined in the text (code) section only.
        m = CODE_SYMBOL_LINE.match(line)
        if not m:
            continue

        pc = int(m.group('pc'), 16)
        size = int(m.group('size'), 16)
        sym_info[pc:pc + size] = Symbol(pc, size, m.group('name'))

    return sym_info
