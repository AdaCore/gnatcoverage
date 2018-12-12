"""
GDB helpers to debug gnatcov.

This package provides helpers to enhance productivity when debugging gnatcov
under GDB. For now it only provides pretty-printers for common types.
"""

import gdb

from gnatcov_gdb.printers import register_printers


_setup_done = False

def setup():
    global _setup_done
    if not _setup_done:
        register_printers(gdb.current_objfile())
        _setup_done = True
