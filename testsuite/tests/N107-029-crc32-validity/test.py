"""
This test sanity checks that the CRC32 value computed by gnatcov for the
executable file for an example program, stored in excution trace headers,
matches the CRC32 value computed by a reference implementation on the same
executable. The Python library is used as the reference implementation.
"""

import binascii

from SUITE.context import thistest
from SUITE.cutils import lines_of, Wdir
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


# Work in a subdirectory
wd = Wdir(subdir='tmp_')

# Generate the program executable, then run it to get an execution trace:
gpr = gprfor(mains=['foo.adb'], srcdirs=['..'])
gprbuild(project=gpr)

program_path = exepath_to('foo')
trace_name = tracename_for('foo')

xrun(program_path)

# Fetch the CRC32 value computed by gnatcov and stored in the trace header as:
#
#   Tag  : EXEC_FILE_CRC32
#   Data :  <decimal checksum>
xcov(['dump-trace', trace_name], out='trace.dump')
trace_dump = [line.strip() for line in lines_of('trace.dump')]

tag_line_no = trace_dump.index('Tag  : EXEC_FILE_CRC32')
data_line = trace_dump[tag_line_no + 2]

# Get the decimal checksum
prefix = 'Data :  '
thistest.stop_if(
    not data_line.startswith(prefix),
    ValueError('Invalid trace text dump (invalid "Data" line): {!r}'
               .format(data_line))
)
gnatcov_checksum = int(data_line[len(prefix):])

# And then compare it with the one Python computes
with open(program_path, 'rb') as f:
    # binascii.crc32 returns a signed 32-bit integer. Make it unsigned to be
    # consistent with GNATcoverage.
    expected_checksum = binascii.crc32(f.read()) & 0xffffffff
thistest.stop_if(
    gnatcov_checksum != expected_checksum,
    ValueError('CRC32 mismatch: {} (expected) != {} (got)'
               .format(expected_checksum, gnatcov_checksum)))

thistest.result()
