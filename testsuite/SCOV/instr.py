"""
Helpers to test instrumentation-based source coverage.
"""

import collections
import os.path

from SUITE.tutils import XCOV, exepath_to, xcov

def xcov_instrument(gprsw, covlevel, isi_file, extra_args=[],
                    dump_method='main-end', out=None,
                    err=None, register_failure=True):
    """
    Run "gnatcov instrument" on a project.

    :param GPRswitches gprsw: Project file command line switches to honor.
    :param str covlevel: Coverage level for the instrumentation
        (--level argument).
    :param str isi_file: Name of the ISI file to create.
    :param list[str] extra_args: Extra arguments to append to the command line.
    :param bool dump_method: Method to dump coverage buffers (--dump-method)
        argument.

    See SUITE.tutils.xcov for the other supported options.
    """
    args = (['instrument', '--level', covlevel, '--dump-method', dump_method] +
            gprsw.as_strings +
            extra_args + [isi_file])
    xcov(args, out=out, err=err, register_failure=register_failure)
