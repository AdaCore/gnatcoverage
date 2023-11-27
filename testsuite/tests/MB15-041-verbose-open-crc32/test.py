from binascii import crc32
import os.path
import re

from e3.os.fs import unixpath

from SUITE.cutils import Wdir, FatalError, lines_of
from SUITE.tutils import (exepath_to, gprbuild, gprfor, thistest,
                          tracename_for, xcov, xrun)

tmp_ = Wdir('tmp_')

GPR_FILE = gprfor('foo.adb', srcdirs='..')
EXE_FILE = exepath_to('foo')
TRACE_FILE = tracename_for('foo')
SCOS_FILES = [
    os.path.join('obj', 'foo.ali'),
]

CWD_LINE_PREFIX = 'CWD = '
LOG_OPEN_RE = re.compile(
    r'\[GNATCOV\.MISC\] --- notice: open "(?P<file>[^"]+)"'
    r' \(CRC32 = 0x(?P<crc32>[0-9a-f]{8})\)'
)


def list_to_text(items):
    return ''.join(
        '  - {}\n'.format(item)
        for item in items
    ) or '  <empty list>\n'


def check_same_files(expected, found):
    expected = set(unixpath(path) for path in expected)
    found = set(unixpath(path) for path in found)
    thistest.fail_if(
        expected != found,
        'Expecting:\n{}'
        'but found:\n{}'.format(
            list_to_text(expected), list_to_text(found)
        )
    )


def check_logging(log_file, expected_files):
    """
    Check that `log_file` contains correct checksums for all `expected_files`,
    and only for them.
    """
    # Rebase all input path to the temporary directory.
    base = os.getcwd()
    expected_files = set(
        os.path.join(base, path)
        for path in expected_files
    )

    checksums = {}
    gnatcov_cwd = None

    # First, get GNATcov's CWD and all the CRC32 checksums from the verbose
    # logging.
    for line in lines_of(log_file):
        if line.startswith(CWD_LINE_PREFIX):
            gnatcov_cwd = line[len(CWD_LINE_PREFIX):].strip()

        else:
            m = LOG_OPEN_RE.match(line)
            if not m:
                continue

            thistest.stop_if(
                not gnatcov_cwd,
                FatalError("Got a checksum before GNATcov's CWD")
            )

            filepath = os.path.join(gnatcov_cwd, m.group('file'))
            checksums[filepath] = int(m.group('crc32'), 16)

    # Check that these checksums match the set of files.
    check_same_files(expected_files, set(checksums.keys()))

    # Then check that each checksum is valid.
    for filename, checksum in checksums.items():
        with open(filename, 'rb') as fp:
            expected_checksum = crc32(fp.read()) & 0xffffffff
        thistest.fail_if(
            expected_checksum != checksum,
            'Bad checksum for {}: expecting CRC32={:#08x}, but found {:#08x}'
            .format(filename, expected_checksum, checksum)
        )


# Build the project
gprbuild(GPR_FILE)

# Produce some trace and check the verbose logging.
xrun(
    ['--verbose', '--level=stmt+mcdc', '-P{}'.format(GPR_FILE), EXE_FILE],
    out='xcov-run.log'
)
check_logging('xcov-run.log', [EXE_FILE] + SCOS_FILES)

# Then compute the coverage report and check again the verbose logging.
xcov(
    ['coverage', '--verbose', '--level=stmt+mcdc', '--annotate=report',
        '-P{}'.format(GPR_FILE), TRACE_FILE],
    out='xcov-coverage.log'
)
check_logging('xcov-coverage.log', [EXE_FILE, TRACE_FILE] + SCOS_FILES)

thistest.result()
