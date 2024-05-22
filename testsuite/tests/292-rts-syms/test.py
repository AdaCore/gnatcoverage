"""
Check that all external symbols exported by gnatcov_rts are prefixed
with "gnatcov_rts" to avoid conflicts with user code.
"""

import os

from e3.os.fs import which

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import contents_of, exename_for, run_and_log, xcov

tmp = Wdir("tmp_")

nm = which(exename_for("nm"))

gcvrt_prefix = "gnatcov_rts"

# Setup gnatcov locally so we don't need to search for the archive
xcov(["setup", "--prefix", gcvrt_prefix])

gnatcov_rts_lib = os.path.join(
    gcvrt_prefix, "lib", "gnatcov_rts.static", "libgnatcov_rts.a"
)

symbol_file = "gnatcov_rts_symbols.txt"
run_and_log(
    [
        nm,
        "-g",  # Only print external symbols
        "--defined-only",  # Only print symbols defined in the library
        "-j",  # Only print the symbol name
        gnatcov_rts_lib,
    ],
    output=symbol_file,
)

symbols = contents_of(symbol_file).splitlines()
for symbol in symbols:
    thistest.fail_if(
        not symbol.startswith("gnatcov_rts"),
        comment=f"gnatcov_rts exports a symbol not prefixed by 'gnatcov_rts'"
        f": {symbol}",
    )

thistest.result()
