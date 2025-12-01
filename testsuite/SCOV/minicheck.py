"""
Dummy XCOV reports checker.

This is a temporary module to help testcases to check coverage analysis
reports. Ideally, we should use the regular SCOV circuitry for that but that's
not possible at the moment for manual report production schemes (for instance
for specific checkpoints usage testcases).
"""

import collections
import glob
import json
import os.path
import re

from e3.fs import rm

from SCOV.instr import (
    default_dump_channel,
    maybe_copy_runtime,
    maybe_relocate_binaries,
    xcov_convert_base64,
    xcov_instrument,
)
from SUITE.cutils import contents_of, indent
from SUITE.tutils import (
    exename_for,
    exepath_to,
    gprbuild,
    run_cov_program,
    srctrace_pattern_for,
    thistest,
    tracename_for,
    xcov,
    xrun,
    GNATCOV_INFO,
)


COV_RE = re.compile(r"^ *(\d+) (.):.*$")


def build_and_run(
    gprsw,
    covlevel,
    mains,
    extra_coverage_args,
    quiet=True,
    scos=None,
    gpr_obj_dir=None,
    gpr_exe_dir=None,
    ignored_source_files=None,
    separate_coverage=None,
    extra_args=None,
    extra_run_args=None,
    extra_instr_args=None,
    extra_gprbuild_args=None,
    extra_gprbuild_cargs=None,
    extra_gprbuild_largs=None,
    absolute_paths=False,
    dump_trigger="auto",
    dump_channel="auto",
    check_gprbuild_output=False,
    trace_mode=None,
    runtime_project=None,
    gprsw_for_coverage=None,
    scos_for_run=True,
    register_failure=True,
    program_env=None,
    tolerate_instrument_messages=None,
    exec_args=None,
    manual_prj_name=None,
    auto_config_args=True,
):
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
    :param bool quiet: Whether to pass "--quiet" to "gnatcov.
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
    :param list[str] | None ignored_source_files: List of file patterns to pass
        using the --excluded-source-files option.
    :param None|str separate_coverage: If provided, the argument is forwarded
        to gnatcov using the -S option.
    :param list[str] | None extra_args: List of arguments to pass to any
        execution of gnatcov (gnatcov run|instrument|coverage).
    :param list[str] extra_run_args: List of arguments to pass to all
        executions of "gnatcov run".
    :param list[str] extra_instr_args: List of arguments to pass to all
        executions of "gnatcov instrument".
    :param list[str] | None extra_gprbuild_args: List of arguments to pass to
        gprbuild.
    :param list[str] | None extra_gprbuild_cargs: List of arguments to pass to
        gprbuild's -cargs section.
    :param list[str] | None extra_gprbuild_largs: List of arguments to pass to
        gprbuild's -largs section.
    :param bool absolute_paths: If true, use absolute paths in the result.
    :param None|str dump_trigger: See xcov_instrument.
    :param None|str dump_channel: See xcov_instrument.
    :param bool check_gprbuild_output: If true, check that gprbuild's output is
        empty.
    :param None|str trace_mode: If None, use the testsuite's trace mode.
        Otherwise, use the given trace mode ('bin', or 'src').
    :param None|str runtime_project: If None, use the default name for the
        instrumentation runtime project. Otherwise, use the name given for this
        option.
    :param None|SUITE.gprutils.GPRswitches gprsw_for_coverage: GPRswitches
        instance used to describe the project and units of interest to analyze
        in "gnatcov coverage". If left to None, use "gprsw".
    :param bool scos_for_run: Whether to pass SCOs/project information to
        "gnatcov run".
    :param bool register_failure: If true and the execution of one of the mains
        exits with a non-zero status code, stop with a FatalError.
    :param None|dict[str, str] program_env: If not none, environment variables
        for the program to run.
    :param None|str tolerate_instrument_messages: If not None, a re pattern
        of error or warning messages tolerated in the "gnatcov instrument"
        output.
    :param None|list[str] exec_args: List of arguments to pass to the
        executable. This will only work for native configurations.
    :param None|str manual_prj_name: When the dump trigger is manual, several
        traces files (one per project) can be emitted if there are dump buffers
        procedure calls in at least two distinct projects. This is the name of
        the project for which we want to consider traces.
    :param bool auto_config_args: If False, do not pass the --config argument
        to gprbuild and gnatcov invocations.

    :rtype: list[str]
    :return: Incomplete list of arguments to pass to `xcov` in order to run
        "gnatcov coverage". The only mandatory argument that is missing is the
        annotation format. The last N arguments correspond to trace files for
        the given N mains. Upon return, the progam execution log for each main
        M is available as M_output.txt.
    """

    def abspath(path):
        return os.path.abspath(path) if absolute_paths else path

    def exepath(main):
        main = os.path.join(
            gpr_exe_dir,
            (os.path.join(gprsw.subdirs, main) if gprsw.subdirs else main),
        )
        return abspath(exepath_to(main))

    def gprbuild_wrapper(root_project):
        # Honor build relevant switches from gprsw here
        gprbuild(
            root_project,
            gargs=gprsw.build_switches + (extra_gprbuild_args or []),
            extracargs=extra_gprbuild_cargs or [],
            largs=extra_gprbuild_largs or [],
            trace_mode=trace_mode,
            runtime_project=runtime_project,
            auto_config_args=auto_config_args,
        )

        if check_gprbuild_output:
            gprbuild_out = contents_of("gprbuild.out")
            thistest.fail_if(
                gprbuild_out,
                "gprbuild's output (gprbuild.out) is not empty:\n{}".format(
                    indent(gprbuild_out)
                ),
            )

    # When instrumenting, we expect units of interest to be provided
    # through GPR switches:
    assert not (scos and trace_mode == "src")

    extra_args = extra_args or []

    gpr_exe_dir = gpr_exe_dir or "."
    gpr_obj_dir = gpr_obj_dir or os.path.join(gpr_exe_dir, "obj")

    trace_mode = trace_mode or thistest.options.trace_mode

    # Use a --level=<l> form for --level to faciliate locating and
    # replacing the switch at once as a whole if need be.
    covlevel_args = [] if covlevel is None else ["--level={}".format(covlevel)]

    xcov_args = ["coverage"] + covlevel_args
    trace_files = []

    # Arguments to pass to "gnatcov coverage" (bin trace mode) or "gnatcov
    # instrument" (src trace mode), in addition to those conveyed by gprsw.
    ignored_source_files = ignored_source_files or []
    cov_or_instr_args = extra_args + [
        "--excluded-source-files={}".format(pattern)
        for pattern in ignored_source_files
    ]
    if separate_coverage:
        cov_or_instr_args.extend(["-S", separate_coverage])
    if quiet:
        cov_or_instr_args.append("--quiet")

    # Compute arguments to specify units of interest.
    if trace_mode == "bin":
        scos_arg = "--scos"
        scos_ext = "ali"
    else:
        scos_arg = "--sid"
        scos_ext = "sid"
    scos = (
        ["{}={}.{}".format(scos_arg, abspath(a), scos_ext) for a in scos]
        if scos
        else gprsw.cov_switches
    )

    out_file_ = "{}_output.txt"

    if trace_mode == "bin":
        # Build and run each main
        gprbuild_wrapper(gprsw.root_project)
        run_args = covlevel_args + extra_args

        if scos_for_run:
            run_args.extend(scos)
        if extra_run_args:
            run_args.extend(extra_run_args)

        eargs = []
        if exec_args:
            eargs = ["-eargs"] + exec_args

        for m in mains:
            out_file = out_file_.format(m)
            xrun(
                run_args + [exepath(m)] + eargs,
                out=out_file,
                env=program_env,
                register_failure=register_failure,
            )
        trace_files = [abspath(tracename_for(m)) for m in mains]

        xcov_args.extend(cov_or_instr_args)

    elif trace_mode == "src":
        # Deal with --dump-trigger=manual,<file>
        is_manual = (
            dump_trigger.split(",")[0] == "manual" if dump_trigger else False
        )

        if dump_channel == "auto":
            dump_channel = default_dump_channel()

        # The AAMP target does not support library project and requires
        # rebuilding the instrumentation runtime: copy it in the test
        # directory.
        maybe_copy_runtime(os.getcwd())

        # Instrument the project and build the result
        extra_instr_args = cov_or_instr_args + list(extra_instr_args or [])
        xcov_instrument(
            gprsw,
            covlevel,
            quiet=False,
            extra_args=extra_instr_args,
            gpr_obj_dir=gpr_obj_dir,
            dump_trigger=dump_trigger,
            dump_channel=dump_channel,
            runtime_project=runtime_project,
            out="instrument.log",
            register_failure=register_failure,
            tolerate_messages=tolerate_instrument_messages,
            auto_config_args=auto_config_args,
        )
        gprbuild_wrapper(gprsw.root_project)

        # Retrieve the dump_channel that "gnatcov instrument" actually used,
        # when available. It could be unavailable when either
        #
        # - the version of gnatcov we run doesn't dump the parameters it
        #   used (older than the introduction of gnatcov setup, typically
        #   in qualification contexts),
        #
        # - or we are called in a context causing a failure on purpose
        #   (register_failure False), at a point before gnatcov has dumped
        #   the parameters.

        params_file_dir = gpr_obj_dir
        if gprsw.subdirs:
            params_file_dir = os.path.join(params_file_dir, gprsw.subdirs)
        params_file = os.path.join(params_file_dir, "gnatcov-instr.json")
        try:
            f = open(params_file)
        except FileNotFoundError:
            actual_dump_channel = None
        else:
            with f:
                params = json.load(f)
            actual_dump_channel = params["dump-channel"]

        # Fail if we expected to be able to retrieve the dump channel
        # actually used, but don't have it:
        thistest.fail_if(
            register_failure
            and GNATCOV_INFO.has_setup
            and actual_dump_channel is None,
            "Unable to retrieve actual dump_channel from {}".format(
                params_file
            ),
        )

        # At this point, dump_channel is either None (request not to pass an
        # argument at all), or a meaningful value that was passed to gnatcov
        # instrument, possibly inferred from an "auto" selection at our level.

        # If an explicit dump channel was provided to gnatcov instrument and
        # we have the actual dump channel used, the two should be consistent:
        thistest.fail_if(
            dump_channel
            and actual_dump_channel
            and dump_channel != actual_dump_channel,
            "requested dump_channel ({}) != actual ({})".format(
                dump_channel, actual_dump_channel
            ),
        )

        # Now execute each main and collect the trace files we can. Tests
        # triggering instrumentation failures on purpose are not guaranteed to
        # produce a trace.

        # See if we know the dump-channel that was used. Sometimes we don't,
        # e.g. from a test failing to instrument or from a test requesting not
        # to pass a dump-channel switch (dump_channel None) with a pre-setup
        # version of gnatcov that would fallback to a default but not dump the
        # parameters it used.

        known_channel = dump_channel or actual_dump_channel

        # Remove potential existing source trace files: the name is
        # non-deterministic by default, so we want to avoid getting
        # multiple traces in the current directory.
        for m in mains:
            rm(srctrace_pattern_for(m, is_manual, manual_prj_name))
            # Callback to run for each instrumented main
            maybe_relocate_binaries(gpr_obj_dir, gpr_exe_dir, [exename_for(m)])

        patterns = set()
        trace_files = []
        for m in mains:
            out_file = out_file_.format(m)
            run_cov_program(
                exepath(m),
                out=out_file,
                env=program_env,
                register_failure=register_failure,
                exec_args=exec_args,
            )

            # See if we have a trace file at hand or if could create one from
            # a base64 trace in the output. Operate best effort here, simply
            # gathering what we can.

            # The possible combinations of gnatcov versions and project file
            # contents associated with dump_channel None on entry together
            # with tests checking for some kinds of failures on purpose make
            # it very tricky to determine what we actually expect at this
            # particular spot.
            #
            # Encoding that logic here is not worth the effort/complexity;
            # simply assume that if we fail to get a trace when our context
            # expects one, there will be some kind of test failure afterwards.

            if known_channel in [None, "bin-file"]:
                patterns.add(
                    srctrace_pattern_for(m, is_manual, manual_prj_name)
                )

            elif (
                known_channel == "base64-stdout"
                or "source trace file ==" in contents_of(out_file)
            ):
                # Pick a trace name that is compatible with srctracename_for
                src_pattern = srctrace_pattern_for(
                    m, is_manual, manual_prj_name
                )
                patterns.add(src_pattern)
                trace_file = src_pattern.replace("*", "-" + m)

                # Here we're really supposed to have a trace in the output
                # so we can be a tad stricter on the conversion outcome.
                xcov_convert_base64(
                    out_file, trace_file, register_failure=register_failure
                )

        # Expand the list of patterns
        if patterns:
            for pattern in patterns:
                trace_files.extend(glob.glob(pattern))

        xcov_args.extend(cov_or_instr_args)

    else:
        raise AssertionError("Unknown trace mode: {}".format(trace_mode))

    # If provided, pass "gnatcov coverage"-specific project arguments, which
    # replace the list of SCOS.
    if gprsw_for_coverage:
        xcov_args.extend(gprsw_for_coverage.cov_switches)
    elif scos:
        xcov_args.extend(scos)

    return xcov_args + extra_coverage_args + list(trace_files)


def build_run_and_coverage(
    out="coverage.log",
    err=None,
    register_failure=True,
    auto_config_args=True,
    tolerate_coverage_messages=None,
    **kwargs,
):
    """
    Helper to call build_and_run and then invoke `xcov`.

    This invokes `xcov` with the command-line that build_and_run returns to
    perform the "gnatcov coverage" step.

    `out` and `err` are forwarded to `xcov`, `register_failure` and
    `auto_config_args` are reported to `xcov` and `build_and_run`, other
    arguments are forwarded to `build_and_run`.

    If tolerate_coverage_messages is not None, filter out error or warning
    messages from the "gnatcov coverage" output matching the regular
    expression.
    """
    xcov_args = build_and_run(
        register_failure=register_failure,
        auto_config_args=auto_config_args,
        **kwargs,
    )
    xcov(
        xcov_args,
        auto_config_args=auto_config_args,
        out=out,
        err=err,
        register_failure=register_failure,
        tolerate_messages=tolerate_coverage_messages,
    )


def checked_xcov(args, out_file):
    """
    Run "xcov" and make the testcase fail if the output file is not empty.
    """
    xcov(args, out_file)
    out = contents_of(out_file)
    thistest.fail_if(
        out,
        "gnatcov output not empty ({}):\n"
        "   {}\n"
        "{}".format(out_file, " ".join(args), out),
    )


def fmt_cov(cov_data):
    """
    Format coverage data into a human readable form.

    This can be used to report differences between expected/got coverage
    reports.
    """
    result = []
    for cov_char in sorted(cov_data):
        result.append(
            "{}({})".format(
                cov_char,
                ", ".join(
                    str(lineno) for lineno in sorted(cov_data[cov_char])
                ),
            )
        )
    return " ".join(result)


def check_xcov_content(filename, expected_cov, trace_mode=None):
    """
    Dumbed-down version of coverage matching. Check that the XCOV file
    "filename" matches some expected coverage data.

    "expected_cov" is a dict like:

    >>> {'+': {5, 7}, '!': {6}}

    This is interpreted as: lines 5 and 7 must be fully covered (+), line 6
    must be partially covered (!) and all other lines must be no-code (.) or
    fully covered (+).

    The recognized coverage annotation are partially covered (!),
    not covered (-), undetermined coverage (?), disabled coverage (D),
    exempted with no violations (#) or exempted with at least one
    violation (*).

    """

    def remove_empty_sets(data):
        """
        Remove entries in "data" that contain empty sets of lines.
        """
        return {
            annotation: lines for annotation, lines in data.items() if lines
        }

    # Check that expected coverage data contain only supported line annotations
    invalid_line_annotations = set(expected_cov) - {
        "+",
        "!",
        "-",
        "?",
        "D",
        "#",
        "*",
    }
    assert not invalid_line_annotations, "Invalid line annotations: {}".format(
        " ".join(sorted(invalid_line_annotations))
    )

    got_cov = collections.defaultdict(set)
    dot_lines = set()
    with open(filename) as f:
        for line in f:
            m = COV_RE.match(line)
            if m:
                lineno, cov_char = m.groups()
                lineno = int(lineno)
                if cov_char == ".":
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
    for line in got_cov.get("+", set()):
        if line not in expected_non_dot_lines:
            refined_expectations["+"].add(line)

    got_cov = remove_empty_sets(got_cov)
    refined_expectations = remove_empty_sets(refined_expectations)

    thistest.fail_if(
        got_cov != refined_expectations,
        "{}: unexpected coverage report content:\n"
        "Expected:   {}\n"
        "Refined to: {}\n"
        "But got:    {}\n".format(
            filename,
            fmt_cov(expected_cov),
            fmt_cov(refined_expectations),
            fmt_cov(got_cov),
        ),
    )


def check_xcov_reports(reports_dir, expected_cov, discard_empty=None):
    """
    Check the set of XCOV report files and their content.

    This checks that the set of "*.xcov" files in the directory "reports_dir"
    matches files mentionned in "expected_cov" and that each report matches the
    expected coverage result.

    "expected_cov" is a mapping: filename -> coverage data. See
    "check_xcov_content" for the coverage data format.

    "discard_empty" is used to adjust test expectations for binary traces:

    * When True, source file for which there are no coverage expectation are
      discarded (binary traces have a technical limitation: gnatcov is unable
      to generate coverage reports for SCO-less source files: see
      eng/das/cov/gnatcoverage#245).

    * When False, reports of source files with no coverage expectations are
      checked.

    * If left to None, "discard_empty" is treated as True when the testsuite
      runs in binary traces mode, and treated as False when the testsuite runs
      in source traces mode.

    None is the most useful default. Tests that force the use of source traces
    regardless of the testsuite trace mode should pass discard_empty=True.
    Tests that have source files showing up as completely no code with binary
    traces should pass discard_empty=False and adjust "expected_cov" depending
    on the testsuite trace mode.
    """

    def fmt_sorted_indented_list(items):
        return "\n".join("  {}".format(s) for s in sorted(items))

    # Avoid discrepancies between filenames on Windows and Unix. Although it is
    # not the canonical representation, Windows supports using slash as
    # separators, so use it.
    def canonicalize_file(filename):
        return filename.replace("\\", "/")

    if discard_empty or (
        discard_empty is None and thistest.options.trace_mode == "bin"
    ):
        expected_cov = {
            filename: expectations
            for filename, expectations in expected_cov.items()
            if any(lines for lines in expectations.values())
        }

    xcov_files = {
        canonicalize_file(filename)
        for filename in os.listdir(reports_dir)
        if filename.endswith(".xcov")
    }
    expected_cov = {
        canonicalize_file(filename): cov_data
        for filename, cov_data in expected_cov.items()
    }

    thistest.fail_if(
        xcov_files != set(expected_cov),
        "Unexpected XCOV files. Expected:\n"
        f"{fmt_sorted_indented_list(expected_cov)}\n"
        "But got instead:\n"
        f"{fmt_sorted_indented_list(xcov_files)}\n",
    )

    for filename, cov_data in expected_cov.items():
        if filename in xcov_files:
            check_xcov_content(os.path.join(reports_dir, filename), cov_data)
