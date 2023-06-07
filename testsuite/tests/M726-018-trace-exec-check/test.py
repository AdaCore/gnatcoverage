from collections import namedtuple
import os
import shutil

from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import (exename_for, exepath_to, tracename_for, gprbuild,
                          gprfor, xrun, xcov,)

# Work in a subdirectory
wd = Wdir(subdir='tmp_')

gpr = gprfor(mains=['foo.adb', 'bar.adb'],
             srcdirs=['..'])

# Executable used to generate the reference trace
expected_program = exename_for('foo')

# Executable that is different
unexpected_program = exename_for('bar')

# Executable that is corrupted
corrupted_program = exename_for('foo-corrupted')

# Real name for the executable used by the trace, convenient for substituing
# other executables still keeping the original one.
program_name = exename_for('program')
program_path = exepath_to('program')
trace_name = tracename_for('program')

# Prepare the test material
gprbuild(project=gpr)

expected_timestamp = (os.stat(expected_program).st_mtime, ) * 2
unexpected_timestamp = (expected_timestamp[0] + 10, ) * 2

shutil.copy(expected_program, corrupted_program)

# Add a 'A' byte to expected_program, and a 'B' byte to the corrupted_program.
# This way, they will have the same size, but a different checksum.
with open(expected_program, 'ab') as f:
    f.write(b'\x00')
with open(corrupted_program, 'ab') as f:
    f.write(b'\x01')

# Generate the reference trace
shutil.copy(expected_program, program_name)
os.utime(program_name, expected_timestamp)
xrun(program_path)

# Actually run the tests.
Case = namedtuple('Case', 'program timestamp expect_warning label')
for case in (
    Case(expected_program,   expected_timestamp, False,
         'GOOD filesize, timestamp, checksum'),
    Case(expected_program,   unexpected_timestamp, True,
         'GOOD program, checksum; BAD timestamp'),
    Case(unexpected_program, expected_timestamp, True,
         'GOOD timestamp, checksum; BAD program'),
    Case(corrupted_program,  expected_timestamp, True,
         'GOOD program, timestamp; BAD checksum'),
):
    shutil.copy(case.program, program_name)
    os.utime(program_name, case.timestamp)

    process = xcov(
        'coverage -P{} -c stmt -a report {}'.format(gpr, trace_name),
        err='log.err', register_failure=False
    )

    xcov_warned = contents_of('log.err').startswith('warning: executable file')

    def expected_repr(expected):
        return 'expected' if expected else 'unexpected'

    thistest.fail_if(
        xcov_warned != case.expect_warning,
        '{} ({})'.format(
            'Warnings expected, got no one'
            if case.expect_warning else
            'Expected no warnings, but got one',

            case.label
        )
    )

thistest.result()
