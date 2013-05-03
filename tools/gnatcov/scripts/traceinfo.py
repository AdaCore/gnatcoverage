# -*- coding: utf-8 -*-

import re
import subprocess

import intervalmap


TRACE_LINE = re.compile(b'^([0-9a-f]+)-([0-9a-f]+) ')


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

    # Mapping: start PC -> end PC (for each trace)
    ranges = {}

    # First collect all traces ranges, unifying ranges that start at the same
    # address.
    for line in outs.split(b'\n'):
        m = TRACE_LINE.match(line)
        if m:
            pc_start = int(m.group(1), 16)
            # For gnatcov, the end address is executed, but for our interval
            # map, the end bound is not covered.
            pc_end = int(m.group(2), 16) + 1

            # If pc_start is already in the range, just take the longest trace.
            try:
                old_end = ranges[pc_start]
            except KeyError:
                ranges[pc_start] = pc_end
            else:
                ranges[pc_start] = max(pc_end, old_end)

    # Then flatten the mapping to an interval map, for fast access. These two
    # steps are needed since IntervalMap objects do not handle overlapping
    # intervals.
    result = intervalmap.IntervalMap()
    last_interval = None
    for pc_start in sorted(ranges):
        pc_end = ranges[pc_start]

        # Depending on overlapping, extend previous interval or add a new
        # interval to the result.
        if last_interval:
            if last_interval[1] < pc_start:
                result[last_interval[0]:last_interval[1]] = True
                last_interval = (pc_start, pc_end)
            else:
                last_interval = (
                    last_interval[0],
                    max(last_interval[1], pc_end)
                )
        else:
            last_interval = (pc_start, pc_end)

    if last_interval:
        result[last_interval[0]:last_interval[1]] = True

    return result


if __name__ == '__main__':
    import sys
    for (pc_start, pc_end), _ in get_trace_info(sys.argv[1]).items():
        print('{:x}-{:x}'.format(pc_start, pc_end - 1))
