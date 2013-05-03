# -*- coding: utf-8 -*-

import collections
import re
import subprocess

import intervalmap


TRACE_LINE = re.compile(
    b'^(?P<start>[0-9a-f]+)-(?P<end>[0-9a-f]+) .: [0-9a-f]{2}'
    b' (?P<special>-|t)(?P<fault>-|t)'
    b'(?P<fallthrough>-|t)(?P<branch>-|t)'
    b' block$'
)


LeaveFlags = collections.namedtuple(
    'LeaveFlags',
    'special fault block fallthrough branch'
)

def merge_flags(f1, f2):
    """Return the merge of two leave flags."""
    return LeaveFlags(*(
        flag1 or flag2 for flag1, flag2 in zip(f1, f2)
    ))


def get_trace_info(traces):
    """Parse trace info from `traces`. Return an interval map whose covered
    elements are executed instructions addresses.
    """
    # Let gnatcov parse the traces for us.
    proc = subprocess.Popen(
        ['gnatcov', 'dump-trace', traces], stdout=subprocess.PIPE
    )

    outs, errs = proc.communicate()
    if proc.returncode != 0:
        raise RuntimeError('gnatcov dump-trace returned an error')

    # Parse its output and collect information.

    # Mapping: end PC -> LeaveFlags
    leave_flags = {}
    # Mapping: start PC -> end PC (for each trace)
    ranges = {}

    # First collect all traces ranges, unifying ranges that start at the same
    # address.
    for line in outs.split(b'\n'):
        m = TRACE_LINE.match(line)
        if m:
            pc_start = int(m.group('start'), 16)
            # For gnatcov, the end address is executed, but for our interval
            # map, the end bound is not covered.
            pc_end = int(m.group('end'), 16) + 1

            flags = LeaveFlags(
                m.group('special') == 't',
                m.group('fault') == 't',
                True,
                m.group('fallthrough') == 't',
                m.group('branch') == 't',
            )

            # If pc_start is already in the range, just take the longest trace.
            try:
                old_end = ranges[pc_start]
            except KeyError:
                ranges[pc_start] = pc_end
            else:
                ranges[pc_start] = max(pc_end, old_end)

            try:
                old_flags = leave_flags[pc_end]
            except KeyError:
                leave_flags[pc_end] = flags
            else:
                leave_flags[pc_end] = merge_flags(old_flags, flags)

    # Then flatten the mapping to an interval map, for fast access. These two
    # steps are needed since IntervalMap objects do not handle overlapping
    # intervals.
    executed_insns = intervalmap.IntervalMap()
    last_interval = None
    for pc_start in sorted(ranges):
        pc_end = ranges[pc_start]

        # Depending on overlapping, extend previous interval or add a new
        # interval to the result.
        if last_interval:
            if last_interval[1] < pc_start:
                executed_insns[last_interval[0]:last_interval[1]] = True
                last_interval = (pc_start, pc_end)
            else:
                last_interval = (
                    last_interval[0],
                    max(last_interval[1], pc_end)
                )
        else:
            last_interval = (pc_start, pc_end)

    if last_interval:
        executed_insns[last_interval[0]:last_interval[1]] = True

    return (executed_insns, leave_flags)


if __name__ == '__main__':
    import sys
    executed_insns, leave_flags = get_trace_info(sys.argv[1])
    for (pc_start, pc_end), _ in executed_insns.items():
        print('{:x}-{:x}: {}'.format(
            pc_start, pc_end - 1, leave_flags[pc_end]
        ))
