"""Helpers to test instrumentation-based source coverage."""

import os.path
import re

from e3.fs import mkdir

from SUITE.context import thistest
from SUITE.cutils import contents_of, ext, indent
from SUITE.tutils import RUNTIME_INFO, xcov


def default_dump_trigger(mains):
    """Return the default dump trigger to use in testcases."""
    if RUNTIME_INFO.has_full_runtime:
        return 'atexit'

    # It does not make sense to have a dump-trigger=ravenscar-task-termination
    # if the main is not an Ada program.
    elif (all([ext(main) == 'adb' for main in mains])
          and RUNTIME_INFO.has_ravenscar_runtime):
        return 'ravenscar-task-termination'
    else:
        return 'main-end'


def default_dump_channel():
    """Return the default dump channel to use in testcases."""
    if RUNTIME_INFO.has_full_runtime:
        return 'bin-file'
    else:
        return 'base64-stdout'


def xcov_instrument(gprsw, covlevel, extra_args=[], dump_trigger="auto",
                    dump_channel="auto", gpr_obj_dir=None,
                    runtime_project=None, out=None, err=None,
                    warnings_as_errors=True, register_failure=True,
                    auto_config_args=True, auto_target_args=True,
                    auto_languages=True):
    """
    Run "gnatcov instrument" on a project.

    :param GPRswitches gprsw: Project file command line switches to honor.
    :param None|str covlevel: Coverage level for the instrumentation
        (--level argument). Not passed if None.
    :param list[str] extra_args: Extra arguments to append to the command line.
    :param None|str dump_trigger: If None, do not pass the --dump-trigger
        argument. If "auto", pass the result of default_dump_trigger().
        Otherwise, pass the given value.
    :param None|str dump_channel: If None, do not pass the --dump-channel
        argument. If "auto", pass the result of default_dump_channel().
        Otherwise, pass the given value.
    :param None|str gpr_obj_dir: Optional name of the directory where gprbuild
        will create build artifacts. If left to None, assume they are produced
        in the current directory.
    :param None|str runtime_project: If None, use the default name for the
        instrumentation runtime project. Otherwise, use the name given for this
        option.
    :param bool warnings_as_errors: Whether to make the test fail if there are
        warnings in gnatcov's output.
    :param bool register_failure: See SUITE.tutils.xcov.
    :param bool auto_config_args: See SUITE.tutils.xcov.
    :param bool auto_target_args: See SUITE.tutils.xcov.
    :param bool auto_languages: See SUITE.tutils.xcov.

    See SUITE.tutils.xcov for the other supported options.
    """
    # Create the object directory so that gnatcov does not warn that it
    # does not exist. This is specific to the source trace mode because
    # we run gnatcov before gprbuild.
    if gpr_obj_dir:
        mkdir(gpr_obj_dir)

    covlevel_args = [] if covlevel is None else ['--level', covlevel]

    # We want to get the mains to know which dump-trigger should be passed to
    # the instrumentation command.
    #
    # Capture the list of main file names, double quoted and comma separated.
    m = re.search(
        pattern=r"for Main use \((?P<mains>.*)\)",
        string=contents_of(gprsw.root_project))

    # If found, split then remove whitespaces and double quotes
    mains = []
    if m:
        mains = m.group('mains').split(',')
        mains = [main.strip(' "') for main in mains]

    args = ['instrument'] + covlevel_args

    if dump_trigger:
        if dump_trigger == "auto":
            dump_trigger = default_dump_trigger(mains)
        args += ["--dump-trigger", dump_trigger]
    if dump_channel:
        if dump_channel == "auto":
            dump_channel = default_dump_channel()
        args += ["--dump-channel", dump_channel]
    if runtime_project:
        args += ["--runtime-project", runtime_project]

    args += gprsw.cov_switches + extra_args

    if thistest.options.pretty_print:
        args.append('--pretty-print')

    out = out or "instrument.log"
    result = xcov(
        args,
        out=out,
        err=err,
        register_failure=register_failure,
        auto_config_args=auto_config_args,
        auto_target_args=auto_target_args,
        auto_languages=auto_languages,
    )

    if register_failure and warnings_as_errors:
        output = contents_of(out)

        thistest.fail_if(
            "***" in output or "!!!" in output or "warning:" in output,
            f"Warnings/errors detected in the output of 'gnatcov instrument':"
            f"\n{indent(output)}"
        )

    return result


def xcov_convert_base64(base64_file, output_trace_file, out=None, err=None,
                        register_failure=True):
    """Extract a trace file out of a Base64 file.

    :param str base64_file: Name of the file to read.
    :param str output_trace_file: Name of the file to write.

    See SUITE.tutils.xcov for the other supported options.
    """
    xcov(['extract-base64-trace', base64_file, output_trace_file],
         out=out, err=err, register_failure=register_failure)


def add_last_chance_handler(project, obj_dir, subdirs, main_unit, silent):
    """
    Add a unit to instrumented sources to hold a last chance handler.

    Several mechanisms provide last chance handlers in cross setups. First,
    libsupport.gpr provides a last chance handler (last_chance.adb) that prints
    a "!!! ERROR !!!"-like pattern that the testsuite will detect as an error,
    and base.gpr makes sure that the __gnat_last_chance_handler is undefined so
    that the linker picks last_chance.o even though it is not in the main
    compilation closure.

    Second, some tests additionally provide a "silence_last_chance.adb" source
    that defines this symbol, only this time, the handler prints no message.
    Test drivers explicitly WITH this unit, so the linker will not pull
    last_chance.o.

    In instrumentation mode, we need the last chance handler to dump coverage
    buffers, and to do that we want to call the procedure that dumps coverage
    for the test driver closure. So we generate in the actual project
    instrumented source directory ($obj_dir/gnatcov-instr or
    $obj_dir/$subdirs/gnatcov-instr) a last chance handler. Which one precisely
    depends on the ``silent`` argument.

    :param str project: Instrumented project. This can be either the name of
        the project file, or the name of the project.
    :param str obj_dir: Path to the object directory of the instrumented
        project.
    :param None|str subdirs: Value of --subdirs passed to gnatcov and gprbuild.
        None if this argument is not passed.
    :param str main_unit: Name of the main unit for which the handler will call
        the coverage buffers dump routine.
    :param bool silent: Whether the last chance handler should be silent. If
        not, it will print a "!!! ERROR !!!"-like pattern that the testsuite
        will detect as an error.
    """
    # Amend obj_dir according to subdirs, if applicable
    if subdirs:
        obj_dir = os.path.join(obj_dir, subdirs)

    # Make sure we have a lowe-cased project *name* (not a filename)
    project = os.path.basename(project).lower()
    if project.endswith('.gpr'):
        project = project[:-4]

    # Unit that contain helper routines to dump coverage bufers. There is one
    # such unit per main. See instrument-common.ads to know more about the slug
    # computation.
    main_unit_slug = main_unit.replace('z', 'zz')
    auto_dump_unit = 'GNATcov_RTS.Buffers.DB_{}'.format(main_unit_slug)
    handler_unit = 'Silent_Last_Chance' if silent else 'Last_Chance'

    def filename(prefix, ext):
        return os.path.join(obj_dir, '{}-gnatcov-instr'.format(project),
                            '{}.{}'.format(prefix, ext))

    unit_prefix = handler_unit.lower()
    with open(filename(unit_prefix, 'ads'), 'w') as f:
        f.write("""
        with System;

        package {unit_name} is
           procedure Last_Chance_Handler
             (Msg : System.Address; Line : Integer);
           pragma Export
             (C, Last_Chance_Handler, "__gnat_last_chance_handler");
           pragma No_Return (Last_Chance_Handler);
        end {unit_name};
        """.format(unit_name=handler_unit))
    with open(filename(unit_prefix, 'adb'), 'w') as f:
        f.write("""
        with System;
        with GNAT.IO;
        with {auto_dump_unit};

        package body {unit_name} is
           procedure Last_Chance_Handler
             (Msg : System.Address; Line : Integer)
           is
              pragma Unreferenced (Msg, Line);
              procedure C_abort;
              pragma Import (C, C_abort, "abort");
              pragma No_Return (C_abort);
           begin
              if not {silent} then
                 GNAT.IO.New_Line;
                 GNAT.IO.Put_Line ("!!!!!!!!!!!!!!!!!!!!!!!!");
                 GNAT.IO.Put_Line ("!!! EXCEPTION RAISED !!!");
                 GNAT.IO.Put_Line ("!!!!!!!!!!!!!!!!!!!!!!!!");
              end if;
              {auto_dump_unit}.Dump_Buffers;
              C_Abort;
           end Last_Chance_Handler;
        end {unit_name};
        """.format(unit_name=handler_unit,
                   auto_dump_unit=auto_dump_unit,
                   silent=silent))

    # Add a "with" to this handler in the main to make sure the handler unit is
    # included in the link.
    main_file = filename(main_unit, 'adb')
    with open(main_file, 'r') as f:
        lines = f.read().splitlines()

    # Insert the "with" clause after all pragmas to keep the code valid
    for i, line in enumerate(lines):
        if not line.strip().lower().startswith('pragma'):
            break
    else:
        assert False, 'Could not find a non-pragma line'
    lines.insert(i, 'with {};'.format(handler_unit))

    with open(main_file, 'w') as f:
        f.write('\n'.join(lines))


def available_ada_dump_triggers():
    """
    Return the list of dump triggers available for the current Ada runtime.
    """
    if RUNTIME_INFO.has_full_runtime:
        return ["main-end", "atexit"]
    elif RUNTIME_INFO.has_ravenscar_runtime:
        return ["main-end", "ravenscar-task-termination"]
    else:
        return ["main-end"]
