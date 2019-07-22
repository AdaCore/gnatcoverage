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

from gnatpython.ex import Run

from SCOV.instr import xcov_instrument
from SUITE.cutils import contents_of
from SUITE.tutils import (exepath_to, gprbuild, srctracename_for, thistest,
                          tracename_for, xcov, xrun)


COV_RE = re.compile('^ *(\d+) (.):.*$')


def build_and_run(gprsw, covlevel, mains, extra_coverage_args, scos=None,
                  gpr_obj_dir=None, gpr_exe_dir=None, ignored_source_files=[],
                  separate_coverage=None, extra_args=[],
                  extra_gprbuild_args=[], absolute_paths=False):
    """
    Prepare a project to run a coverage analysis on it.

    This is a wrapper around gprbuild/xrun/xcov_instrument to do whatever is
    necessary for the current trace mode to run "gnatcov coverage". It lets one
    write concise testcases that handle both binary and source trace modes.

    :param SUITE.gprutils.GPRswitches gprsw: GPRswitches instance used to
        describe the project and units of interest to analyze.
    :param str covlevel: Coverage level (as passed with gnatcov's --level=
        argument) for the coverage analysis.
    :param list[str] mains: List of names for the various mains to run. These
        are lower-case names without extension, for instance "foo" for the
        "foo.adb" main source file.
    :param list[str] extra_coverage_args: List of arguments to append to the
        "gnatcov coverage" command-line returned. This is just for convenience:
        one can pass an empty list here and manually append extra arguments to
        the result.
    :param None|list[str] scos: Optional list of SCOs to pass to gnatcov, if
        the current trace mode is binary. If absent, we pass "-P" to "gnatcov
        coverage"/"gnatcov instrument" so that it automatically discovers the
        units of interest.
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
    :param list[str] extra_gprbuild_args: List of arguments to pass to
        gprbuild.
    :param bool absolute_paths: If true, use absolute paths in the result.

    :rtype: list[str]
    :return: Incomplete list of arguments to pass to `xcov` in order to run
        "gnatcov coverage". The only mandatory argument that is missing is the
        annotation format. The last N arguments correspond to trace files for
        the given N mains.
    """
    def abspath(path):
        return os.path.abspath(path) if absolute_paths else path

    def exepath(main):
        if gpr_exe_dir:
            main = os.path.join(gpr_exe_dir, m)
        return abspath(exepath_to(main))

    def gprbuild_wrapper(root_project, gargs=[]):
        gprbuild(root_project, gargs=gargs + extra_gprbuild_args)

    gpr_exe_dir = gpr_exe_dir or '.'
    gpr_obj_dir = gpr_obj_dir or os.path.join(gpr_exe_dir, 'obj')

    trace_mode = thistest.options.trace_mode
    xcov_args = ['coverage', '--level', covlevel]
    trace_files = []

    # Arguments to pass to "gnatcov coverage" (bin trace mode) or "gnatcov
    # instrument" (src trace mode).
    cov_or_instr_args = (
        extra_args +
        ['--ignore-source-files={}'.format(pattern)
         for pattern in ignored_source_files])
    if separate_coverage:
        cov_or_instr_args.extend(['-S', separate_coverage])

    if trace_mode == 'bin':
        # Compute arguments to specify units of interest
        scos = (['--scos={}'.format(abspath(a)) for a in scos]
                     if scos else
                     gprsw.as_strings)
        cov_or_instr_args.extend(scos)

        # Build and run each main
        gprbuild_wrapper(gprsw.root_project)
        for m in mains:
            xrun(['--level', covlevel, exepath(m)] + scos +
                  extra_args,
                 out='run.log')
        trace_files = [abspath(tracename_for(m)) for m in mains]

        xcov_args.extend(cov_or_instr_args)

    elif trace_mode == 'src':
        # Instrument the project and build the result
        isi_file = abspath('instr.isi')
        xcov_instrument(gprsw, covlevel, isi_file,
                        extra_args=cov_or_instr_args, gpr_obj_dir=gpr_obj_dir,
                        out='instrument.log')
        xcov_args.extend(['--isi', isi_file])
        gprbuild_wrapper(gprsw.root_project,
                         gargs=['--src-subdirs=gnatcov-instr'])

        # Then execute each main
        for m in mains:
            Run([exepath(m)])
        trace_files = [abspath(srctracename_for(m)) for m in mains]

        # If we would have passed the project to "gnatcov coverage" in binary
        # trace mode, pass it here too. This is necessary for instance to
        # select the default output directory for coverage reports.
        if not scos:
            xcov_args.extend(['-P', gprsw.root_project])

        xcov_args.extend(extra_args)

    else:
        assert False, 'Unknown trace mode: {}'.format(trace_mode)

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
    xcov_args = build_and_run(**kwargs)
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


def check_xcov_reports(xcov_filename_pattern, expected_cov):
    """
    Check the set of XCOV report files and their content.

    Collect files that match "xcov_filename_pattern" (a glob pattern) and check
    the set of files matches "expected_cov". Then, check that each report
    matches the expected coverage results.

    "expected_cov" is a mapping: filename -> coverage data. See
    "check_xcov_content" for the coverage data format.
    """

    def fmt_sorted_indented_list(items):
        return '\n'.join('  {}'.format(s) for s in sorted(items))

    # Avoid discrepancies between filenames on Windows and Unix. Although it is
    # not the canonical representation, Windows supports using slash as
    # separators, so use it.
    def canonicalize_file(filename):
        return filename.replace('\\', '/')

    xcov_files = {canonicalize_file(filename)
                  for filename in glob.glob(xcov_filename_pattern)}
    expected_cov = {canonicalize_file(filename): cov_data
                    for filename, cov_data in expected_cov.iteritems()}

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
