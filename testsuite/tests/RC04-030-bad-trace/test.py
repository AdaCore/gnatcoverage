"""
Check that, with --keep-reading-traces, when processing malformed trace files,
gnatcov emits diagnostics that are useful to identify which trace file is
malformed. Also check that the computation of the coverage report go to
completion, and that gnatcov still exits with an error status code.
"""

import os
import os.path
import re

from SCOV.minicheck import check_xcov_reports
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.tutils import (exepath_to, gprfor, gprbuild, thistest,
                          tracename_for, xcov, xrun)


def indexes(string, substr):
    """
    Return the list of all indexes in "string" at which the given substring can
    be found.
    """
    result = []
    start = 0
    while start < len(string):
        i = string.find(substr, start)
        if i == -1:
            break
        result.append(i)
        start = i + 1
    return result


tmp = Wdir('tmp_')
os.mkdir('traces')

mains = ('main_a', 'main_b', 'main_c')
tracefiles = [os.path.join('traces', tracename_for(m)) for m in mains]

prj = gprfor(['{}.adb'.format(m) for m in mains], srcdirs='..')
gprbuild(prj)

# Produce trace files for all mains
for main, tracefile in zip(mains, tracefiles):
    xrun([exepath_to(main),
          '-o', tracefile,
          '-T', 'Trace for {}'.format(main)])

# Corrupt the trace for main_b. Look for the second trace header and truncate
# the file in the middle of a trace entry. We go through this to get an error
# that is consistent across runs.
with open(tracefiles[1], 'rb') as f:
    data = f.read()
header_indexes = indexes(data, b'#QEMU-Traces')
header_size = 20  # See Qemu_Traces.Trace_Header
with open(tracefiles[1], 'wb') as f:
    f.write(data[:header_indexes[1] + header_size + 5])


def gnatcov_coverage(keep_reading_traces):
    log_file = 'coverage-{}.log'.format(
        'krt' if keep_reading_traces else 'default')

    argv = ['coverage', '-axcov', '-cstmt', '-P{}'.format(prj)]
    if keep_reading_traces:
        argv.append('--keep-reading-traces')

    p = xcov(argv + tracefiles, out=log_file, register_failure=False)
    thistest.fail_if(p.status == 0,
                     '"gnatcov coverage" status code is 0 (error expected)')

    coverage_log = contents_of(log_file).strip()
    expected_log = re.compile(
        # Regexp to accommodate output differences between the various
        # supported platforms.
        '[^\n]*gnatcov[^\n]*: traces[/\\\\]main_b[^\n]*.trace: file truncated')
    thistest.fail_if(not expected_log.match(coverage_log),
                     'Unexpected output for "gnatcov coverage". Expected:\n'
                     '{}\n'
                     'but got:\n'
                     '{}'.format(indent(expected_log.pattern),
                                 indent(coverage_log)))


# Make sure that without --keep-reading-traces, no coverage report is produced
gnatcov_coverage(False)
check_xcov_reports('obj/*.xcov', {})


def in_obj(filename):
    return os.path.join('obj', filename)


# Make sure that with it, both the first and the last trace files contributed
# to coverage assessment, while the middle one was skipped.
gnatcov_coverage(True)
check_xcov_reports(
    in_obj('*.xcov'),
    {in_obj('pkg.adb.xcov'): {'+': {11, 12, 14, 17, 18},
                              '-': {15}},
     in_obj('main_a.adb.xcov'): {'+': {5}},
     in_obj('main_b.adb.xcov'): {'-': {5}},
     in_obj('main_c.adb.xcov'): {'+': {5}}}
)

thistest.result()
