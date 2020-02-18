"""Helpers to test instrumentation-based source coverage."""

from gnatpython.fileutils import mkdir

from SUITE.tutils import xcov



def default_dump_trigger():
    """Return the default dump trigger to use in testcases."""
    return 'atexit'


def xcov_instrument(gprsw, covlevel, extra_args=[],
                    dump_trigger=None, gpr_obj_dir=None, out=None,
                    err=None, register_failure=True):
    """
    Run "gnatcov instrument" on a project.

    :param GPRswitches gprsw: Project file command line switches to honor.
    :param None|str covlevel: Coverage level for the instrumentation
        (--level argument). Not passed if None.
    :param list[str] extra_args: Extra arguments to append to the command line.
    :param None|str dump_trigger: Trigger to dump coverage buffers
        (--dump-trigger argument). If left to None,
        use SCOV.instr.default_dump_trigger.
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
    args = (['instrument'] + covlevel_args +
            ['--dump-trigger', dump_trigger or default_dump_trigger()] +
            gprsw.as_strings +
            extra_args)
    xcov(args, out=out, err=err, register_failure=register_failure)


def xcov_convert_base64(base64_file, output_trace_file, out=None, err=None,
                        register_failure=True):
    """Extract a trace file out of a Base64 file.

    :param str base64_file: Name of the file to read.
    :param str output_trace_file: Name of the file to write.

    See SUITE.tutils.xcov for the other supported options.
    """
    xcov(['extract-base64-trace', base64_file, output_trace_file],
         out=out, err=err, register_failure=register_failure)
