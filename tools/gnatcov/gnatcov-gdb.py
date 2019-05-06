"""
GDB hook to load GDB helpers to debug gnatcov.

See scripts/gnatcov_gdb/__init__.py for more information about the helpers
themselves.

There are two ways to use them: load this file directly from GDB:

    (gdb) source /path/to/this/gnatcov-gdb.py

... and that's all: the pretty-printers are registered and you can start
debugging.

The other alternative is to let GDB load it automatically for you. For this to
work you need to create a symbolic link to this file in the same directory as
the "gnatcov" program you are debugging. You also need to add the corresponding
directory to the "auto-load safe-path": see
<https://sourceware.org/gdb/onlinedocs/gdb/objfile_002dgdbdotext-file.html> for
more details.
"""

# Make the gnatcov_gdb package available
import os.path
import sys
sys.path.append(os.path.join(
    os.path.dirname(os.path.realpath(os.path.expanduser(__file__))),
    'scripts'))

import gnatcov_gdb
gnatcov_gdb.setup()
