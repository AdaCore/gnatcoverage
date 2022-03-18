"""
Dummy XCOV reports checker.

This is a temporary module to help testcases to check coverage analysis
reports. Ideally, we should use the regular SCOV circuitry for that but that's
not possible at the moment for manual report production schemes (for instance
for specific checkpoints usage testcases).
"""

import collections
import glob
import os.path
import re

from e3.fs import rm

from SCOV.instr import (default_dump_channel, xcov_convert_base64,
                        xcov_instrument)
from SUITE.cutils import contents_of, indent
from SUITE.tutils import (
    exepath_to, gprbuild, run_cov_program, srctrace_pattern_for,
    srctracename_for, thistest, tracename_for, xcov, xrun
)


COV_RE = re.compile(r'^ *(\d+) (.):.*$')


def build_and_run(gprsw, covlevel, mains, extra_coverage_args, scos=None,
                  gpr_obj_dir=None, gpr_exe_dir=None, ignored_source_files=[],
                  separate_coverage=None, extra_args=[], extra_run_args=None,
                  extra_instr_args=None, extra_gprbuild_args=[],
                  extra_gprbuild_cargs=[], absolute_paths=False,
                  dump_trigger=None, dump_channel=None,
                  check_gprbuild_output=False, trace_mode=None,
                  gprsw_for_coverage=None, scos_for_run=True,
                  register_failure=True, program_env=None,
                  instrument_warnings_as_errors=True, exec_args=None):
    """
    Prepare a project to run a coverage analysis on it.

    This is a wrapper around gprbuild/xrun/xcov_instrument to do whatever is
    necessary for the current trace mode to run "gnatcov coverage". It lets one
    write concise testcases that handle both binary and source trace modes.

    :param SUITE.gprutils.GPRswitches gprsw: GPRswitches instance used to
        describe the project and units of interest to analyze.
    :param None|str covlevel: Coverage level (as passed with gnatcov's --level=
        argument) for the coverage analysis. Not passed if None.
    :param list[str] mains: List of names for the various mains to run. These
        are lower-case names without extension, for instance "foo" for the
        "foo.adb" main source file.
    :param list[str] extra_coverage_args: List of arguments to append to the
        "gnatcov coverage" command-line returned. This is just for convenience:
        one can pass an empty list here and manually append extra arguments to
        the result.
    :param None|list[str] scos: Optional list of SCOs files (ALI or SID) must
        be passed to gnatcov. These files must have no extension (for instance:
        'obj/foo' instead of 'obj/foo.ali'. If absent, we pass "-P" to "gnatcov
        coverage"/"gnatcov instrument" so that it automatically discovers the
        units of interest from projects.
    :param None|str gpr_obj_dir: Optional name of the directory where gprbuild
        will create build artifacts. If left to None, assume they are produced
        in "$gpr_exe_dir/obj".
    :param None|str gpr_exe_dir: Optional name of the directory where gprbuild
        will create executables to run. If left to None, assume they are
        produced in the current directory.
    :param list[str] ignored_source_files: List of file patterns to pass using
        the --ignore-source-files option.
    :param None|str separate_coverage: If provided, the argument is forwarded
        to gnatcov using the -S option.
    :param list[str] extra_args: List of arguments to pass to any
        execution of gnatcov (gnatcov run|instrument|coverage).
    :param list[str] extra_run_args: List of arguments to pass to all
        executions of "gnatcov run".
    :param list[str] extra_instr_args: List of arguments to pass to all
        executions of "gnatcov instrument".
    :param list[str] extra_gprbuild_args: List of arguments to pass to
        gprbuild.
    :param list[str] extra_gprbuild_cargs: List of arguments to pass to
        gprbuild's -cargs section.
    :param bool absolute_paths: If true, use absolute paths in the result.
    :param None|str dump_trigger: Trigger to dump coverage buffers
        (--dump-trigger argument). If left to None,
        use SCOV.instr.default_dump_trigger.
    :param None|str dump_channel: Channel to dump coverage buffers
        (--dump-channel argument). If left to None,
        use SCOV.instr.default_dump_channel.
    :param bool check_gprbuild_output: If true, check that gprbuild's output is
        empty.
    :param None|str trace_mode: If None, use the testsuite's trace mode.
        Otherwise, use the given trace mode ('bin', or 'src').
    :param None|SUITE.gprutils.GPRswitches gprsw_for_coverage: GPRswitches
        instance used to describe the project and units of interest to analyze
        in "gnatcov coverage". If left to None, use "gprsw".
    :param bool scos_for_run: Whether to pass SCOs/project information to
        "gnatcov run".
    :param bool register_failure: If true and the execution of one of the mains
        exits with a non-zero status code, stop with a FatalError.
    :param None|dict[str, str] program_env: If not none, environment variables
        for the program to run.
    :param bool instrument_warnings_as_errors: Whether to make the test fail if
        there are warnings in the output of "gnatcov instrument".
    :param None|list[str] exec_args: List of arguments to pass to the
        executable. This will only work for native configurations.

    :rtype: list[str]
    :return: Incomplete list of arguments to pass to `xcov` in order to run
        "gnatcov coverage". The only mandatory argument that is missing is the
        annotation format. The last N arguments correspond to trace files for
        the given N mains.
    """
    def abspath(path):
        return os.path.abspath(path) if absolute_paths else path

    def exepath(main):
        main = os.path.join(
            gpr_exe_dir,
            (os.path.join(gprsw.subdirs, main) if gprsw.subdirs else main))
        return abspath(exepath_to(main))

    def gprbuild_wrapper(root_project, gargs):

        # Honor build relevant switches from gprsw here
        gprbuild(root_project,
                 gargs=gprsw.build_switches + gargs + extra_gprbuild_args,
                 extracargs=extra_gprbuild_cargs,
                 trace_mode=trace_mode)

        if check_gprbuild_output:
            gprbuild_out = contents_of('gprbuild.out')
            thistest.fail_if(
                gprbuild_out,
                "gprbuild's output (gprbuild.out) is not empty:\n{}"
                .format(indent(gprbuild_out)))

    # When instrumenting, we expect units of interest to be provided
    # through GPR switches:
    assert not (scos and trace_mode == 'src')

    gpr_exe_dir = gpr_exe_dir or '.'
    gpr_obj_dir = gpr_obj_dir or os.path.join(gpr_exe_dir, 'obj')

    trace_mode = trace_mode or thistest.options.trace_mode

    # Use a --level=<l> form for --level to faciliate locating and
    # replacing the switch at once as a whole if need be.
    covlevel_args = [] if covlevel is None else ['--level={}'.format(covlevel)]

    xcov_args = ['coverage'] + covlevel_args
    trace_files = []

    # Arguments to pass to "gnatcov coverage" (bin trace mode) or "gnatcov
    # instrument" (src trace mode), in addition to those conveyed by gprsw.
    cov_or_instr_args = (
        extra_args +
        ['--ignore-source-files={}'.format(pattern)
         for pattern in ignored_source_files])
    if separate_coverage:
        cov_or_instr_args.extend(['-S', separate_coverage])

    # Compute arguments to specify units of interest.
    if trace_mode == 'bin':
        scos_arg = '--scos'
        scos_ext = 'ali'
    else:
        scos_arg = '--sid'
        scos_ext = 'sid'
    scos = (['{}={}.{}'.format(scos_arg, abspath(a), scos_ext)
             for a in scos]
            if scos else
            gprsw.cov_switches)

    if trace_mode == 'bin':
        # Build and run each main
        gprbuild_wrapper(gprsw.root_project, gargs=[])
        run_args = covlevel_args + extra_args

        if scos_for_run:
            run_args.extend(scos)
        if extra_run_args:
            run_args.extend(extra_run_args)

        eargs = []
        if exec_args:
            eargs = ["-eargs"] + exec_args

        for m in mains:
            xrun(run_args + [exepath(m)] + eargs, out='run.log',
                 env=program_env, register_failure=register_failure)
        trace_files = [abspath(tracename_for(m)) for m in mains]

        xcov_args.extend(cov_or_instr_args)

    elif trace_mode == 'src':
        dump_channel = dump_channel or default_dump_channel()

        # Instrument the project and build the result
        extra_instr_args = cov_or_instr_args + list(extra_instr_args or [])
        xcov_instrument(gprsw, covlevel, extra_args=extra_instr_args,
                        gpr_obj_dir=gpr_obj_dir, dump_trigger=dump_trigger,
                        dump_channel=dump_channel, out='instrument.log',
                        register_failure=register_failure,
                        warnings_as_errors=instrument_warnings_as_errors)
        gprbuild_wrapper(gprsw.root_project,
                         gargs=['--src-subdirs=gnatcov-instr'])

        # Then execute each main and collect trace files
        trace_files = []
        for m in mains:
            # Remove potential existing source trace files: the name is
            # non-deterministic by default, so we want to avoid getting
            # multiple traces in the current directory.
            rm(srctrace_pattern_for(m))

            out_file = '{}_output.txt'.format(m)
            run_cov_program(exepath(m), out=out_file, env=program_env,
                            register_failure=register_failure,
                            exec_args=exec_args)

            # Depending on the dump channel, we also may have to create the
            # trace file.
            if dump_channel == 'bin-file':
                trace_file = srctracename_for(m,
                                              register_failure=register_failure)
                if trace_file is None:
                    continue

            elif dump_channel == 'base64-stdout':
                # Create a trace name that is compatible with srctracename_for
                trace_file = srctrace_pattern_for(m).replace("*", "unique")

                xcov_convert_base64(out_file, trace_file,
                                    register_failure=register_failure)

            else:
                raise ValueError('Invalid dump channel: {}'
                                 .format(dump_channel))

            trace_files.append(abspath(trace_file))

        xcov_args.extend(cov_or_instr_args)

    else:
        assert False, 'Unknown trace mode: {}'.format(trace_mode)

    # If provided, pass "gnatcov coverage"-specific project arguments, which
    # replace the list of SCOS.
    if gprsw_for_coverage:
        xcov_args.extend(gprsw_for_coverage.cov_switches)
    elif scos:
        xcov_args.extend(scos)

    return xcov_args + extra_coverage_args + trace_files


def build_run_and_coverage(out='coverage.log', err=None, register_failure=True,
                           **kwargs):
    """
    Helper to call build_and_run and then invoke `xcov`.

    This invokes `xcov` with the command-line that build_and_run returns to
    perform the "gnatcov coverage" step.

    `out`, `err` and `register_failure` are forwarded to `xcov`, other
    arguments are forwarded to `build_and_run`.
    """
    xcov_args = build_and_run(register_failure=register_failure, **kwargs)
    xcov(xcov_args, out=out, err=err, register_failure=register_failure)


def checked_xcov(args, out_file):
    """
    Run "xcov" and make the testcase fail if the output file is not empty.
    """
    xcov(args, out_file)
    out = contents_of(out_file)
    thistest.fail_if(
        out,
        'gnatcov output not empty ({}):\n'
        '   {}\n'
        '{}'.format(out_file, ' '.join(args), out)
    )


def fmt_cov(cov_data):
    """
    Format coverage data into a human readable form.

    This can be used to report differences between expected/got coverage
    reports.
    """
    result = []
    for cov_char in sorted(cov_data):
        result.append('{}({})'.format(
            cov_char, ', '.join(str(lineno)
                                for lineno in sorted(cov_data[cov_char]))
        ))
    return ' '.join(result)


def check_xcov_content(filename, expected_cov):
    """
    Dumbed-down version of coverage matching. Check that the XCOV file
    "filename" matches some expected coverage data.

    "expected_cov" is a dict like:

    >>> {'+': {5, 7}, '!': {6}}

    This is interpreted as: lines 5 and 7 must be fully covered (+), line 6
    must be partially covered (!) and all other lines must be no-code (.) or
    fully covered (+).
    """

    def remove_empty_sets(data):
        """
        Remove entries in "data" that contain empty sets of lines.
        """
        return {annotation: lines
                for annotation, lines in data.items()
                if lines}

    # Check that expected coverage data contain only supported line annotations
    invalid_line_annotations = set(expected_cov) - {'+', '!', '-'}
    assert not invalid_line_annotations, (
        'Invalid line annotations: {}'
        .format(' '.join(sorted(invalid_line_annotations))))

    got_cov = collections.defaultdict(set)
    dot_lines = set()
    with open(filename) as f:
        for line in f:
            m = COV_RE.match(line)
            if m:
                lineno, cov_char = m.groups()
                lineno = int(lineno)
                if cov_char == '.':
                    dot_lines.add(lineno)
                else:
                    got_cov[cov_char].add(lineno)
    got_cov = dict(got_cov)

    # Compute the set of lines that are expected not to be tagged as no-code
    # and refine expectations to expect "+" when we got "+" while we expected
    # nothing specific.
    expected_non_dot_lines = set()
    for lines in expected_cov.values():
        expected_non_dot_lines.update(lines)

    refined_expectations = collections.defaultdict(set)
    refined_expectations.update(expected_cov)
    for line in got_cov.get('+', set()):
        if line not in expected_non_dot_lines:
            refined_expectations['+'].add(line)

    got_cov = remove_empty_sets(got_cov)
    refined_expectations = remove_empty_sets(refined_expectations)

    thistest.fail_if(
        got_cov != refined_expectations,
        '{}: unexpected coverage report content:\n'
        'Expected:   {}\n'
        'Refined to: {}\n'
        'But got:    {}\n'.format(
            filename, fmt_cov(expected_cov), fmt_cov(refined_expectations),
            fmt_cov(got_cov)
        )
    )


def check_xcov_reports(xcov_filename_pattern, expected_cov, cwd=None):
    """
    Check the set of XCOV report files and their content.

    Collect files that match "xcov_filename_pattern" (a glob pattern) and check
    the set of files matches "expected_cov". Then, check that each report
    matches the expected coverage results.

    "expected_cov" is a mapping: filename -> coverage data. See
    "check_xcov_content" for the coverage data format.

    If "cwd" is not None, it must be a valid directory name, and both the
    filename patterns and the file names in expected_cov must be relative to
    it.
    """

    def fmt_sorted_indented_list(items):
        return '\n'.join('  {}'.format(s) for s in sorted(items))

    # Avoid discrepancies between filenames on Windows and Unix. Although it is
    # not the canonical representation, Windows supports using slash as
    # separators, so use it.
    def canonicalize_file(filename):
        return filename.replace('\\', '/')

    home_dir = None
    try:
        if cwd is not None:
            home_dir = os.getcwd()
            os.chdir(cwd)

        xcov_files = {canonicalize_file(filename)
                      for filename in glob.glob(xcov_filename_pattern)}
        expected_cov = {canonicalize_file(filename): cov_data
                        for filename, cov_data in expected_cov.items()}

        thistest.fail_if(
            xcov_files != set(expected_cov),
            'Unexpected XCOV files. Expected:\n'
            '{}\n'
            'But got instead:\n'
            '{}\n'.format(fmt_sorted_indented_list(expected_cov),
                          fmt_sorted_indented_list(xcov_files))
        )

        for filename, cov_data in expected_cov.items():
            if filename in xcov_files:
                check_xcov_content(filename, cov_data)
    finally:
        if home_dir is not None:
            os.chdir(home_dir)
