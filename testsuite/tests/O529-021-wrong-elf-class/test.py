import re

from SUITE.context import thistest
from SUITE.tutils import xcov


# Note: test material was generated with the following commands with GCC for
# x86_64-linux:
#   $ gcc -o code-32.o -m32 -c code.s
#   $ gcc -o code-64.o -m64 -c code.s
bits = '32' if '64bits' in thistest.options.tags else '64'
expected_pattern = (r'.*gnatcov.*: code-{bits}\.o:'
                    r' unsupported ELF class \({bits}bit\)'.format(bits=bits))

p = xcov(['disassemble', 'code-{}.o'.format(bits)], register_failure=False)
got_output = p.out.strip()
thistest.fail_if(
    not re.match(expected_pattern, got_output),
    'Could not match:\n'
    '    {}\n'
    'against output:\n'
    '    {}'.format(expected_pattern, got_output))
thistest.result()
