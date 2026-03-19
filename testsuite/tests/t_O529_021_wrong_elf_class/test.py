from SUITE.context import thistest
from SUITE.tutils import xcov


# Note: test material was generated with the following commands with GCC for
# x86_64-linux:
#   $ gcc -o code-32.o -m32 -c code.s
#   $ gcc -o code-64.o -m64 -c code.s
bits = "32" if "64bits" in thistest.options.tags else "64"
expected_error = (
    f"gnatcov: BINARY_FILES.ERROR: code-{bits}.o: unsupported ELF class"
    f" ({bits}bit)"
)

p = xcov(["disassemble", "code-{}.o".format(bits)], register_failure=False)
got_output = p.out.strip()
thistest.fail_if_not_equal(
    'output of "gnatcov coverage"', expected_error, got_output
)
thistest.result()
