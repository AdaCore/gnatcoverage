"""Helpers to test instrumentation-based source coverage."""

import os.path
import re
import shutil

from e3.fs import mkdir, sync_tree

from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import contents_of, ext, copy_to_dir
from SUITE.tutils import RUNTIME_INFO, GNATCOV_INFO, locate_gpr_file, xcov


def default_dump_trigger(mains):
    """Return the default dump trigger to use in testcases."""
    if thistest.options.default_dump_trigger:
        return thistest.options.default_dump_trigger
    elif RUNTIME_INFO.has_full_runtime:
        return "atexit"

    # It does not make sense to have a dump-trigger=ravenscar-task-termination
    # if the main is not an Ada program.
    elif (
        all(ext(main) == "adb" for main in mains)
        and RUNTIME_INFO.has_ravenscar_runtime
        # TODO: There seems to be a problem with light-tasking runtimes and
        # task-termination, so default back to main end in that case.
        # See eng/cov/gnatcoverage#191
        and "light-tasking" not in RUNTIME_INFO.runtime_name
    ):
        return "ravenscar-task-termination"
    else:
        return "main-end"


def default_dump_channel():
    """Return the default dump channel to use in testcases."""
    if thistest.options.default_dump_channel:
        return thistest.options.default_dump_channel
    elif RUNTIME_INFO.has_full_runtime:
        return "bin-file"
    else:
        return "base64-stdout"


def xcov_instrument(
    gprsw,
    covlevel,
    quiet=True,
    extra_args=None,
    dump_trigger="auto",
    dump_channel="auto",
    gpr_obj_dir=None,
    runtime_project=None,
    out=None,
    err=None,
    tolerate_messages=None,
    register_failure=True,
    auto_config_args=True,
    auto_target_args=True,
):
    """
    Run "gnatcov instrument" on a project.

    :param GPRswitches gprsw: Project file command line switches to honor.
    :param None|str covlevel: Coverage level for the instrumentation
        (--level argument). Not passed if None.
    :param bool quiet: Whether to pass the "--quiet" flag.
    :param list[str] | None extra_args: Extra arguments to append to the
        command line.
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
    :param None|str tolerate_messages: If not None, a re pattern of warning
        or error messsages tolerated in the tool output. Messages not matching
        this pattern will cause a test failure when register_failure is True.
    :param bool register_failure: See SUITE.tutils.xcov.
    :param bool auto_config_args: See SUITE.tutils.xcov.
    :param bool auto_target_args: See SUITE.tutils.xcov.

    See SUITE.tutils.xcov for the other supported options.
    """
    # Create the object directory so that gnatcov does not warn that it
    # does not exist. This is specific to the source trace mode because
    # we run gnatcov before gprbuild.
    if gpr_obj_dir:
        mkdir(gpr_obj_dir)

    covlevel_args = [] if covlevel is None else ["--level", covlevel]

    # We want to get the mains to know which dump-trigger should be passed to
    # the instrumentation command.
    #
    # Capture the list of main file names, double quoted and comma separated.
    m = re.search(
        pattern=r"for Main use \((?P<mains>.*)\)",
        string=contents_of(locate_gpr_file(gprsw)),
    )

    # If found, split then remove whitespaces and double quotes
    mains = []
    if m:
        mains = m.group("mains").split(",")
        mains = [main.strip(' "') for main in mains]

    args = ["instrument"] + covlevel_args
    if quiet:
        args.append("--quiet")

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

    args += gprsw.cov_switches + (extra_args or [])

    if thistest.options.pretty_print:
        args.append("--pretty-print")

    if thistest.options.block:
        args.append("--instrument-block")

    # When no message is to be tolerated, fallback to an actual regexp
    # that will never match:
    re_tolerate_messages = tolerate_messages or ""

    # For qualification purposes, tolerate possible warnings about
    # inexistant object dirs from older gnatcov versions, typically
    # issued when instrumenting before building. Tests do a best
    # effort attempt at creating objects dirs beforehand but doing
    # that is cumbersome for some of the more convoluted tests.
    if not GNATCOV_INFO.major_at_least(24):
        re_tolerate_messages = "|".join(
            "(?:{})".format(mre)
            for mre in ["object directory.*not found", re_tolerate_messages]
        )

    out = out or "instrument.log"
    result = xcov(
        args,
        out=out,
        err=err,
        register_failure=register_failure,
        auto_config_args=auto_config_args,
        auto_target_args=auto_target_args,
        tolerate_messages=re_tolerate_messages,
    )

    return result


def xcov_convert_base64(
    base64_file, output_trace_file, out=None, err=None, register_failure=True
):
    """Extract a trace file out of a Base64 file.

    :param str base64_file: Name of the file to read.
    :param str output_trace_file: Name of the file to write.

    See SUITE.tutils.xcov for the other supported options.
    """
    xcov(
        ["extract-base64-trace", base64_file, output_trace_file],
        out=out,
        err=err,
        register_failure=register_failure,
    )


def add_dumper_lch_hook(project, obj_dir, subdirs, main_unit):
    """
    Add a unit to instrumented sources to hold a last chance handler
    entry hook dumping the coverage buffers for the given main_unit.

    :param str project: Instrumented project. This can be either the name of
        the project file, or the name of the project.
    :param str obj_dir: Path to the object directory of the instrumented
        project.
    :param None|str subdirs: Value of --subdirs passed to gnatcov and gprbuild.
        None if this argument is not passed.
    :param str main_unit: Name of the main unit for which the hook will call
        the coverage buffers dump routine.
    """
    # Isolate the dependency to the libadalang module. This means that a user
    # testing natively (thus not needing the support for last chance handlers)
    # will not need to install the libadalang python module.
    import libadalang as lal

    # Amend obj_dir according to subdirs, if applicable
    if subdirs:
        obj_dir = os.path.join(obj_dir, subdirs)

    # Make sure we have a lowe-cased project *name* (not a filename)
    project = os.path.basename(project).lower()
    if project.endswith(".gpr"):
        project = project[:-4]

    # Unit that contain helper routines to dump coverage buffers. There is one
    # such unit per main. The only differences between two such units of the
    # same project is the name of the main unit which is encoded in the trace
    # (in place of the actual executable name). This is not checked by the
    # testsuite so there should be no problem using the helper unit of a
    # different main.

    auto_dump_suffix = None
    for _, _, files in os.walk(
        os.path.join(obj_dir, f"{project}-gnatcov-instr")
    ):
        for file in files:
            res = re.match(pattern="gcvrt-db_(.+).adb", string=file)
            if res:
                auto_dump_suffix = res.group(1)
                break
        if auto_dump_suffix:
            break

    assert auto_dump_suffix is not None

    # Determine if the buffer dump helper unit corresponds to a manual buffer
    # dump. If so, there is an extra parameter to pass to the call to the
    # Dump_Buffers procedure.
    is_manual_dump = not re.match("z[0-9a-f]+", auto_dump_suffix)

    auto_dump_unit = "GCVRT.DB_{}".format(auto_dump_suffix)
    handler_unit = "Last_Chance_Dumper"

    def filename(prefix, ext):
        return os.path.join(
            obj_dir,
            "{}-gnatcov-instr".format(project),
            "{}.{}".format(prefix, ext),
        )

    unit_prefix = handler_unit.lower()
    with open(filename(unit_prefix, "ads"), "w") as f:
        f.write(
            """
        package {unit_name} is
           procedure Lch_Enter;
           pragma Export (Ada, Lch_Enter, "__lch_enter");
        end {unit_name};
        """.format(
                unit_name=handler_unit
            )
        )
    with open(filename(unit_prefix, "adb"), "w") as f:
        f.write(
            """
        with {auto_dump_unit};

        package body {unit_name} is
           procedure Lch_Enter is
           begin
              {auto_dump_unit}.Dump_Buffers{call_params};
           end;
        end {unit_name};
        """.format(
                unit_name=handler_unit,
                auto_dump_unit=auto_dump_unit,
                call_params=f' ("{main_unit}")' if is_manual_dump else "",
            )
        )

    # Amend the main unit to "with" the generated package so it gets
    # included in the build. Insert the "with" clause after all pragmas
    # to keep the code valid.

    main_file = filename(main_unit, "adb")

    context = lal.AnalysisContext()
    unit = context.get_from_file(main_file)

    # Assume that the root node is a CompilationUnit. Then look for the token
    # right before the start of the body node: this is where we will insert the
    # with_handler clause.
    assert not unit.diagnostics
    assert isinstance(unit.root, lal.CompilationUnit)
    after_body_token = unit.lookup_token(unit.root.f_body.sloc_range.start)
    before_body_token = after_body_token.previous

    # build the new source
    new_source = "".join(
        [
            (
                ""
                if before_body_token is None
                else lal.Token.text_range(unit.first_token, before_body_token)
            ),
            "with {};".format(handler_unit),
            lal.Token.text_range(after_body_token, unit.last_token),
        ]
    )
    with open(main_file, "w") as f:
        f.write(new_source)


def available_ada_dump_triggers():
    """
    Return the list of dump triggers available for the current Ada runtime.
    """
    if RUNTIME_INFO.has_full_runtime:
        return ["main-end", "atexit"]
    elif (
        RUNTIME_INFO.has_ravenscar_runtime
        # TODO: remove once --dump-trigger=ravenscar-task-termination is
        # fixed, see eng/das/cov/gnatcoverage#191.
        and "light-tasking" not in RUNTIME_INFO.runtime_name
    ):
        return ["main-end", "ravenscar-task-termination"]
    else:
        return ["main-end"]


def maybe_relocate_binaries(object_dir, exe_dir, mains):
    """
    Relocate binaries produced in the object_dir to the exe_dir for the AAMP
    target.

    :param object_dir: object directory of the main project.
    :param exe_dir: executable directory of the main project.
    :param mains: main filenames.
    """
    # The GNAAMP linker disregards the full name passed to the -o switch but
    # only picks the simple name. Thus, the executable ends up in the object
    # directory.
    #
    # Copy it to the executable directory so that the rest of the testsuite
    # can assume executables are always there.
    if "aamp" in env.target.platform:
        for main in mains:
            copy_to_dir(object_dir, exe_dir, main)


def maybe_copy_runtime(test_dir):
    """
    Copy the Ada instrumentation runtime in test_dir for the AAMP target.
    """
    if "aamp" in env.target.platform:
        rts_path = os.path.join(
            os.path.dirname(shutil.which("gnatcov")),
            "..",
            "share",
            "gnatcoverage",
            "gnatcov_ada_rts",
        )
        rts_dest_path = os.path.join(test_dir, "rts")
        mkdir(rts_dest_path)
        sync_tree(rts_path, rts_dest_path)
        env.add_search_path(env_var="GPR_PROJECT_PATH", path=rts_dest_path)
