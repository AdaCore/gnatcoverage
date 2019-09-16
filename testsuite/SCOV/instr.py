"""
Helpers to test instrumentation-based source coverage.
"""

import collections
import os.path

from gnatpython.fileutils import mkdir

from SUITE.tutils import XCOV, exepath_to, xcov

def xcov_instrument(gprsw, covlevel, isi_file, extra_args=[],
                    dump_method='atexit', gpr_obj_dir=None, out=None, err=None,
                    register_failure=True):
    """
    Run "gnatcov instrument" on a project.

    :param GPRswitches gprsw: Project file command line switches to honor.
    :param None|str covlevel: Coverage level for the instrumentation
        (--level argument). Not passed if None.
    :param str isi_file: Name of the ISI file to create.
    :param list[str] extra_args: Extra arguments to append to the command line.
    :param str dump_method: Method to dump coverage buffers (--dump-method)
        argument.
    :param None|str gpr_obj_dir: Optional name of the directory where gprbuild
        will create build artifacts. If left to None, assume they are produced
        in the current directory.

    See SUITE.tutils.xcov for the other supported options.
    """
    # Create the object directory so that gnatcov does not warn that it
    # does not exist. This is specific to the source trace mode because
    # we run gnatcov before gprbuild.
    if gpr_obj_dir:
        mkdir(gpr_obj_dir)

    covlevel_args = [] if covlevel is None else ['--level', covlevel]
    args = (['instrument'] + covlevel_args + ['--dump-method', dump_method] +
            gprsw.as_strings +
            extra_args + [isi_file])
    xcov(args, out=out, err=err, register_failure=register_failure)
