# -*- coding: utf-8 -*-

import collections
import os.path
import re
import subprocess

import intervalmap


SLOC_INFO_LINE = re.compile(
    b'^(?P<pc_start>[0-9a-f]+)-(?P<pc_stop>[0-9a-f]+)'
    b' line (?P<src_file>[^:]+):(?P<line>\d+)(?:[:](?P<column>\d+))?'
    b'(?: discriminator (?P<discriminator>\d+))?$'
)
Sloc = collections.namedtuple('Sloc', 'filename line column discriminator')


def format_sloc(sloc, basename=False):
    if sloc is None:
        return 'No source line information'
    filename = sloc.filename.decode('ascii')
    filename = (
        os.path.basename(filename)
        if basename else
        filename
    )
    return '{}:{}{}{}'.format(
        filename, sloc.line,
        '' if sloc.column is None else ':{}'.format(sloc.column),
        ''
        if sloc.discriminator is None else
        ' discriminator {}'.format(sloc.discriminator),
    )


def get_sloc_info(exe_filename):
    """Parse sloc info in `exe_filename` and return it as an interval map.

    The result maps from program counter to lists of `Sloc` objects.
    """
    sloc_info = intervalmap.IntervalMap()

    # Let gnatcov parse ELF and DWARF for us.
    proc = subprocess.Popen(
        ['gnatcov', 'dump-lines', exe_filename], stdout=subprocess.PIPE
    )
    outs, errs = proc.communicate()
    if proc.returncode != 0:
        raise RuntimeError('gnatcov dump-lines returned an error')

    # Used to store only one string per filename (reducing memory consumption).
    strings = {}
    def uniq_string(string):
        try:
            return strings[string]
        except KeyError:
            strings[string] = string
            return string

    def int_or_none(value):
        if value is None:
            return None
        else:
            return int(value)

    for line in outs.split(b'\n'):
        m = SLOC_INFO_LINE.match(line)
        if m:
            pc_start = int(m.group('pc_start'), 16)
            pc_stop = int(m.group('pc_stop'), 16) + 1
            sloc = Sloc(
                uniq_string(m.group('src_file')),
                int_or_none(m.group('line')),
                int_or_none(m.group('column')),
                int_or_none(m.group('discriminator')),
            )
            try:
                sloc_info[pc_start:pc_stop] = [sloc]
            except ValueError:
                sloc_info[pc_start].append(sloc)

    return sloc_info
