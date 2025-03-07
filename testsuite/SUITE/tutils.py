# ***************************************************************************
# **                      TEST-COMMON UTILITY functions                    **
# ***************************************************************************

# This module exposes common utility functions to every test instance.  They
# depend on the current test context and are not suitable for the toplevel
# suite driver.

# ***************************************************************************

from dataclasses import dataclass
import glob
import os
import re
import sys
import time


from e3.fs import cp
from e3.os.fs import touch, unixpath, which
from e3.os.process import Run


# Expose a few other items as a test util facilities as well

from SUITE import control
from SUITE.control import (
    BUILDER,
    KNOWN_LANGUAGES,
    env,
    gnatemu_board_name,
    language_info,
    xcov_pgm,
)
from SUITE.context import ROOT_DIR, thistest


# Then mind our own business

from SUITE.cutils import (
    FatalError,
    contents_of,
    list_to_file,
    text_to_file,
    to_list,
    unhandled_exception_in,
    Wdir,
)


# Precompute some values we might be using repeatedly

TARGET_INFO = control.target_info()
RUNTIME_INFO = control.runtime_info()
GNATCOV_INFO = control.gnatcov_info()

XCOV = xcov_pgm(thistest.options.auto_arch)
VALGRIND = "valgrind" + env.host.os.exeext

MEMCHECK_LOG = "memcheck.log"
CALLGRIND_LOG = "callgrind-{}.log"

# Pattern to match the line in gprls' verbose output that specifies the full
# path to the project file to analyze.
GPRLS_PARSING_RE = re.compile(r'^.*\.gpr: info: Parsing "(.*)"$')

# ----------------------------------------------------------------------------
#                 Notes on program and command execution paths
#
# Our tests execute lots of commands to instrument and build programs, to run
# the programs themselves, to produce reports from coverage traces.
#
# For robustness, we strive to minimize the paths controlling such executions
# within the testsuite. This lets us place important checks at critical spots
# and retain confidence in what we are testing as the testsuite grows.
#
# Of particular interest are the paths used to execute test programs as the
# notion of exit status may vary across execution environments and extra
# checks on the program outputs are sometimes needed to detect some kinds of
# failures.
#
# Here's a sketch of how various functions/methods cooperate for src/bin
# trace modes and cross/native configurations. _Please_ keep the general
# issue in mind when considering changes in this area.
#
#         [test.py(bin)]     [test.py(bin/src)]
#              |             build_and_run
#              v               |  |
#              o-------(bin)---o  o----(src)-------o
#              |                                   |
#              |                                   |
#              |             [driver.py(bin/src)]  |
#              |             mode_execute          |
#              |               |  |                |
#              |  o----(bin)---o  o---(src)----o   |
#              |  |                            |   |
#              v  v                            |   |
#              xrun                            |   |
#               |                              |   |
#               v                              |   |
#              xcov                            v   v
#                |                        run_cov_program
#                |                             |
#                | gnatcov run pgm             | pgm (native)
#                | (now cross only)            | <target>-gnatemu pgm (cross)
#                |                             |
#                o-------------o  o------------o
#                              |  |
#                              v  v
#                             cmdrun
#
# ----------------------------------------------------------------------------

run_processes = []
"""
List of processes run through run_and_log. Useful for debugging.

:type: list[Run]
"""


def run_and_log(*args, **kwargs):
    """
    Wrapper around e3.os.process.Run to collect all processes that are run.
    """
    start = time.time()
    p = Run(*args, **kwargs)

    # Register the command for this process as well as the time it took to run
    # it.
    try:
        cmd = kwargs["cmds"]
    except KeyError:
        cmd = args[0]
    p.original_cmd = cmd
    p.duration = time.time() - start
    run_processes.append(p)

    return p


def gprbuild_gargs_with(thisgargs, trace_mode=None, runtime_project=None):
    """
    Compute and return all the toplevel gprbuild arguments to pass. Account for
    specific requests in THISGARGS.

    If TRACE_MODE is "src", consider that we are building an instrumented
    project even if the testsuite mode tells otherwise.

    If RUNTIME_PROJECT is not null, it will be used as the name of the
    instrumentation runtime project in source trace mode.
    """
    trace_mode = trace_mode or thistest.options.trace_mode

    # Force a few bits useful for practical reasons and without influence on
    # code generation
    result = [
        "-f",  # always rebuild
        "-XSTYLE_CHECKS=",  # style checks off
        "-p",  # create missing directories (obj, typically)
    ]
    result.extend(to_list(thisgargs))

    # If we work with source instrumentation, add the dependency on the
    # instrumentation runtime project so that instrumented programs are
    # compilable in the generated projects. Also use instrumented sources in
    # the "*-gnatcov-instr" object directories.
    if trace_mode == "src":
        runtime_project = runtime_project or RUNTIME_INFO.gnatcov_rts_project
        result += [
            f"--implicit-with={runtime_project}",
            "--src-subdirs=gnatcov-instr",
        ]

    return result


def gprbuild_cargs_with(thiscargs, scovcargs=True, suitecargs=True):
    """
    Compute and return all the cargs arguments to pass on gprbuild invocations,
    in accordance with the gprbuild() documentation.

    For SUITECARGS, include language agnostic and language specific switches
    (-cargs and -cargs:<lang>).
    """

    # Check assumptions made by the production of qualification material,
    # which runs source coverage tests only.

    if thistest.options.qualif_level:
        thistest.stop_if(
            not scovcargs,
            FatalError("SCOV_CARGS required for qualification test"),
        )
        thistest.stop_if(
            thiscargs,
            FatalError("Specific CARGS forbidden for qualification test"),
        )

    # Compute the language specific cargs, from testsuite args first:

    lang_cargs = []

    if suitecargs:
        for lang in KNOWN_LANGUAGES:
            lcargs = to_list(thistest.suite_cargs_for(lang))
            if lcargs:
                lang_cargs.extend(["-cargs:%s" % lang] + lcargs)

    # Add the cargs for SCOV based tests if requested. While these are
    # notionally language agnostic, they are only supported for a subset of
    # languages in practice and could be rejected for languages not in this
    # subset.

    SCOV_CARGS = BUILDER.SCOV_CARGS(thistest.options)

    if scovcargs and SCOV_CARGS:
        for lang in KNOWN_LANGUAGES:
            lang_cargs.extend(["-cargs:%s" % lang] + SCOV_CARGS)

    # Compute the language agnostic cargs: testsuite -cargs + those requested
    # for this specific run.

    other_cargs = []

    if suitecargs:
        other_cargs.extend(to_list(thistest.suite_cargs_for(lang=None)))

    other_cargs.extend(to_list(thiscargs))

    if other_cargs:
        other_cargs = ["-cargs"] + other_cargs

    return lang_cargs + other_cargs


def gprbuild_largs_with(thislargs):
    """
    Compute and return all the largs gprbuild arguments to pass.  Account for
    specific requests in THISLARGS.
    """
    all_largs = to_list(thistest.options.largs)
    all_largs.extend(to_list(thislargs))

    # On Windows, executables are made position independent by default, which
    # gnatcov does not handle, so instruct the linker to not create position
    # independent executables if running in bin trace mode.
    if (
        thistest.env.build.os.name == "windows"
        and thistest.options.trace_mode == "bin"
    ):
        all_largs.append("-no-pie")

    if all_largs:
        all_largs.insert(0, "-largs")

    return all_largs


def gpr_common_args(project, auto_config_args=True):
    """
    Return common GPR tools options for the current testsuite run.

    :param auto_config_args: if False, do not add the --config argument.
    """
    gproptions = []

    # If running in qualif mode, run with the verbose switch  for verifiability
    if thistest.options.qualif_level:
        gproptions.append("-v")
    if auto_config_args:
        gproptions.append(
            "--config={}".format(os.path.join(ROOT_DIR, BUILDER.SUITE_CGPR))
        )

    # Workaround a desynchronization between default build configuration
    # for TMS570 and GNATemulator's settings: see O519-032. We may get rid
    # of this kludge one day adapting GNATemulator.
    if thistest.options.RTS and thistest.options.RTS.endswith("-tms570"):
        gproptions.append("-XLOADER=LORAM")

    # For trace32 runs where the test is executed on a real board, we
    # choose to have both the code and data in RAM. The default is to run
    # from flash which would take more time for the probe to program. It
    # would also wear out the flash memory.
    if (
        thistest.options.gnatcov_run
        and "trace32" in thistest.options.gnatcov_run
    ):
        gproptions.append("-XLOADER=RAM")

    # Add runtime specific scenario variables
    return gproptions + RUNTIME_INFO.gpr_scenario_vars


def gprbuild(
    project,
    scovcargs=True,
    suitecargs=True,
    extracargs=None,
    gargs=None,
    largs=None,
    trace_mode=None,
    runtime_project=None,
    out="gprbuild.out",
    register_failure=True,
    auto_config_args=True,
):
    """
    Cleanup & build the provided PROJECT file using gprbuild, passing
    GARGS/CARGS/LARGS as gprbuild/cargs/largs command-line switches. Each
    of these arguments may be either None, a list of options, or a string
    containing a space-separated list of options.

    SCOVCARGS tell whether or not we should prepend BUILDER.SCOV_CARGS to
    the -cargs switches.

    SUITECARGS tells whether or not we should also add the -cargs passed on
    the testsuite toplevel command line.

    See gprbuild_gargs_with for the meaning of TRACE_MODE and RUNTIME_PROJECT.

    OUT is the name of the file to contain gprbuild's output.

    Stop with a FatalError if the execution status is not zero and
    REGISTER_FAILURE is True. Return the process descriptor otherwise.

    If AUTO_CONFIG_ARGS is False, do not pass the --config argument.
    """

    # Fetch options, from what is requested specifically here
    # or from command line requests
    all_gargs = gprbuild_gargs_with(
        thisgargs=gargs,
        trace_mode=trace_mode,
        runtime_project=runtime_project,
    )
    all_largs = gprbuild_largs_with(thislargs=largs)
    all_cargs = gprbuild_cargs_with(
        scovcargs=scovcargs, suitecargs=suitecargs, thiscargs=extracargs
    )
    common_args = gpr_common_args(project, auto_config_args)

    # Now cleanup, do build and check status
    thistest.cleanup(project, common_args)

    # lookup the hook for the executable name without extension
    builder = thistest.suite_gprpgm_for(
        os.path.splitext(os.path.basename(BUILDER.BASE_COMMAND))[0]
    )

    has_altrun = builder is not None

    if builder is None:
        builder = BUILDER.BASE_COMMAND

    args = (
        to_list(builder)
        + ["-P%s" % project]
        + common_args
        + all_gargs
        + all_cargs
        + all_largs
    )
    # If there is an altrun hook for gprbuild, it may be a script.
    # Instruct the Run primitive to parse the shebang to invoke the correct
    # interpreter in that case.
    p = run_and_log(
        args,
        output=out,
        timeout=thistest.options.timeout,
        parse_shebang=has_altrun,
    )
    if register_failure:
        thistest.stop_if(
            p.status != 0, FatalError("gprbuild exit in error", out)
        )
    return p


def gprinstall(project, gargs=None):
    """
    Run "gprinstall" on the provided project file.

    :param None|list[str] gargs: list of command line switches to pass to
        gprinstall
    """
    ofile = "gprinstall.out"
    args = ["gprinstall", "-P", project, "-p"]

    # Add mandatory options, such as target and RTS info
    args.extend(gpr_common_args(project))

    # Add user-provided arguments
    args.extend(to_list(gargs))

    p = run_and_log(args, output=ofile, timeout=thistest.options.timeout)
    thistest.stop_if(
        p.status != 0, FatalError("gprinstall exit in error", ofile)
    )


def gpr_emulator_package():
    """
    If there is a board name, return a package Emulator to be included in a GPR
    file to provide this information to GNATemulator.
    """
    gnatemu_board = gnatemu_board_name(env.target.machine)
    return (
        "package Emulator is\n"
        '   for Board use "{}";\n'
        "end Emulator;".format(gnatemu_board)
        if gnatemu_board
        else ""
    )


def gprfor(
    mains,
    prjid="gen",
    srcdirs="src",
    objdir=None,
    exedir=".",
    main_cargs=None,
    langs=None,
    deps=None,
    scenario_extra="",
    compiler_extra="",
    extra="",
    cwd=None,
):
    """
    Generate a simple PRJID.gpr project file to build executables for each main
    source file in the MAINS list, sources in SRCDIRS. Inexistant directories
    in SRCDIRS are ignored. Assume the set of languages is LANGS when
    specified; infer from the mains otherwise. Add SCENARIO_EXTRA, if any, at
    the beginning of the project files (for scenario variables). Add
    COMPILER_EXTRA, if any, at the end of the Compiler package contents. Add
    EXTRA, if any, at the end of the project file contents. Return the gpr file
    name.

    If CWD is None, generate the project file in the current directory.
    Generate it in the CWD directory otherwise.

    If PRJID contains any '.', they are replaced by '-' in the project
    filename.
    """

    cwd = cwd or os.getcwd()
    mains = to_list(mains)
    srcdirs = to_list(srcdirs)
    langs = to_list(langs)

    # Fetch the support project file template
    template = contents_of(os.path.join(ROOT_DIR, "template.gpr"))

    # Instanciate the template fields.

    # Turn the list of main sources into the proper comma separated sequence
    # of string literals for the Main GPR attribute.
    gprmains = ", ".join('"%s"' % m for m in mains)
    if gprmains:
        gprmains = "for Main use (%s);" % gprmains

    # Likewise for source dirs. Filter on existence, to allow widening the set
    # of tentative dirs while preventing complaints from gprbuild about
    # inexistent ones.
    srcdirs_list = [d for d in srcdirs if os.path.exists(os.path.join(cwd, d))]

    # Determine the language(s) from the sources if they are not explicitly
    # passed as parameters.
    if not langs:
        lang_infos = [
            language_info(src)
            for srcdir in srcdirs_list
            for src in os.listdir(os.path.join(cwd, srcdir))
        ]
        langs = {li.name for li in lang_infos if li}

    srcdirs = ", ".join('"%s"' % d for d in srcdirs_list)
    languages = ", ".join('"%s"' % lang for lang in langs)

    # In addition to the provided dependencies, figure out if this project
    # should extend or with some support or helper facilities. These are
    # designed with projects for test *programs* in mind, not for libraries,
    # and would actually be plain incompatible with shared Library projects.
    for_library = "Library" in extra

    # The base project file we need to extend, which drags libsupport,
    # and the way to refer to it from the project contents.
    basegpr = (
        "{}/support/base.gpr".format(ROOT_DIR) if not for_library else None
    )

    # For projects with an Ada main, provide visibility on the alternative
    # last chance handlers. Restricting this to Ada mains ensures that the
    # dedicated object file for a given handler only gets included in the
    # closure if the program requests the corresponding unit explicitly via
    # a "with" clause, e.g. "with Silent_Last_Chance;".
    #
    # Account for callers that expect the "deps" argument they provide to
    # remain unmodified, or which provide a tuple on input (unmutable).
    deps = list(deps) if deps else []
    if not for_library and ".adb" in gprmains:
        deps.append("{}/support/lch.gpr".format(ROOT_DIR))

    deps = "\n".join('with "%s";' % dep for dep in deps)

    # If we have specific flags for the mains, append them. This is
    # typically something like:
    #
    #    for Switches("test_blob.adb") use
    #      Compiler'Default_Switches("Ada") & ("-fno-inline")

    compswitches = ""
    if main_cargs:
        compswitches = (
            "\n".join(
                [
                    'for Switches("%s") use \n'
                    '  Compiler\'Default_Switches ("%s") & (%s);'
                    % (
                        main,
                        language_info(main).name,
                        ",".join(
                            ['"%s"' % carg for carg in to_list(main_cargs)]
                        ),
                    )
                    for main in mains
                ]
            )
            + "\n"
        )

    # Now instanciate, dump the contents into the target gpr file and return
    gprtext = template % {
        "prjname": prjid,
        "extends": ('extends "%s"' % basegpr) if basegpr else "",
        "scenario": scenario_extra,
        "srcdirs": srcdirs,
        "exedir": exedir,
        "objdir": objdir or (exedir + "/obj"),
        "compswitches": compswitches,
        "languages": languages,
        "gprmains": gprmains,
        "deps": deps,
        "compiler_extra": compiler_extra,
        "pkg_emulator": gpr_emulator_package(),
        "extra": extra,
    }

    return text_to_file(
        text=gprtext,
        filename=os.path.join(cwd, prjid.replace(".", "-") + ".gpr"),
    )


# The following functions abstract away the possible presence of extensions at
# the end of executable names depending on the target, e.g. ".out" for vxworks.

# PGNNAME is a program name, in the main subprogram name sense. An empty
# PGMNAME is allowed, in which case the functions return only the extensions.


def exename_for(pgmname):
    """Name of the executable for the given program name"""
    return pgmname + TARGET_INFO.exeext


def tracename_for(pgmname):
    """Name for the binary trace file for the given program name"""
    return exename_for(pgmname) + ".trace"


def srctrace_pattern_for(pgmname, manual=False, manual_prj_name=None):
    """
    Glob pattern for the source trace file for the given program name.

    :param bool manual: Indicates if the trace file was created as a result of
        a manual dump buffers procedure call.
    :param None|str manual_prj_name: Trace files emitted in manual dump trigger
        mode contain the name of the relevant project in their name.
        manual_prj_name is the name of the project which trace we want to find.
    """
    return (manual_prj_name if manual else exename_for(pgmname)) + "*.srctrace"


def srctracename_for(
    pgmname, register_failure=True, manual=False, manual_prj_name=None
):
    """
    Name for the source trace file for the given program name.

    Since source trace files is not predictible, we need to do produce the
    source trace file first and then look for a file matching a pattern to find
    it.

    If we find zero or multiple traces and "register_failure" is True, this
    stops the testcase. If "register_failure" is False, we just return None in
    that case.
    """
    pattern = srctrace_pattern_for(pgmname, manual, manual_prj_name)
    trace_files = glob.glob(pattern)

    if len(trace_files) == 1:
        return trace_files[0]

    elif register_failure:
        thistest.stop(
            FatalError(
                "Exactly one trace expected matching:"
                f"\n  {pattern}"
                f"\nBut got {len(trace_files)} traces instead"
            )
        )

    else:
        return None


def ckptname_for(pgmname):
    """Coverage checkpoint name"""
    return exename_for(pgmname) + ".ckpt"


# Those two are very similar. The unix version is mostly useful on Windows for
# tests that are going to search for exe filenames in outputs using regular
# expressions, where backslashes as directory separators introduce confusion.


def exepath_to(pgmname):
    """
    Return the absolute path to the executable file expected in the current
    directory for a main subprogram PGMNAME.
    """
    return os.path.abspath(exename_for(pgmname))


def unixpath_to(pgmname):
    """
    Return the absolute path to the executable file expected in the current
    directory for a main subprogram PGMNAME, unixified.
    """
    return unixpath(os.path.abspath(exename_for(pgmname)))


def maybe_valgrind(command):
    """
    Return the input COMMAND list, wrapped with valgrind or callgrind,
    depending on the options.  If such a wrapper is added, valgrind will have
    to be available for the execution to proceed.
    """
    if not thistest.options.enable_valgrind:
        prefix = []
    elif thistest.options.enable_valgrind == "memcheck":
        prefix = [VALGRIND, "-q", "--log-file=%s" % MEMCHECK_LOG]
    elif thistest.options.enable_valgrind == "callgrind":
        log_file = CALLGRIND_LOG.format(thistest.create_callgrind_id())
        prefix = [
            VALGRIND,
            "-q",
            "--tool=callgrind",
            "--callgrind-out-file=%s" % log_file,
        ]
    else:
        raise ValueError(
            "Invalid Valgrind tool: {}".format(
                thistest.options.enable_valgrind
            )
        )
    return prefix + command


def platform_specific_symbols(symbols):
    """
    Given SYMBOLS, a list of architecture-independant symbol names, return a
    list of corresponding of architecture-specific symbol names.

    For instance for Windows, this prepends an underscore to every symbol name.
    """
    return [TARGET_INFO.to_platform_specific_symbol(sym) for sym in symbols]


def locate_gpr_file(gprswitches):
    """
    Use gprls to locate the GPR file for ``gprswitches``'s root project.
    """
    # Run gprls to let GPR code do the complex project file resolution, enable
    # verbose mode so that it prints the full path to the resolved file.
    filename = gprswitches.root_project
    args = ["gprls", "-P", filename, "-v"] + gprswitches.build_switches
    if thistest.options.target:
        args.append(f"--target={thistest.options.target}")
    if thistest.options.RTS:
        args.append(f"--RTS={thistest.options.RTS}")
    gprls_output = Run(args).out

    # Look for the resolved project filename in the output, complain if we
    # could not find one.
    for line in gprls_output.splitlines():
        m = GPRLS_PARSING_RE.match(line)
        if m is not None:
            return m.group(1)
    raise ValueError(
        f"could not locate {filename} with gprls:\n{gprls_output}"
    )


def xcov_suite_args(
    covcmd,
    covargs,
    auto_config_args=True,
    auto_target_args=True,
    force_project_args=False,
):
    """
    Arguments we should pass to gnatcov to obey what we received on the command
    line, in particular --config and --target/--RTS.

    By default (when FORCE_PROJECT_ARGS is False), pass arguments to correctly
    load a project (--config/--target/--RTS) iff COVARSGS contains a -P option
    (i.e.  iff a project is actually loaded). When FORCE_PROJECT_ARGS is True,
    pass these arguments if respectively AUTO_CONFIG_ARGS and AUTO_TARGET_ARGS
    are True.
    """
    project_handling_enabled = (
        force_project_args
        or covcmd in ("setup", "setup-integration")
        or any(arg.startswith("-P") for arg in covargs)
    )

    result = []

    # If --config is asked and project handling is involved, pass it and stop
    # there. If there is a board, it must be described in the project file
    # (gnatcov's -P argument).
    if project_handling_enabled and auto_config_args:
        result.append(
            "--config={}".format(os.path.join(ROOT_DIR, BUILDER.SUITE_CGPR))
        )
        result.extend(RUNTIME_INFO.gpr_scenario_vars)
        return result

    # If --config is asked and project handling is involved, pass it and stop
    # there. If there is a board, it must be described in the project file
    # (gnatcov's -P argument).
    if auto_config_args and project_handling_enabled:
        return result

    # Nothing to add if the caller does not want automatic --target/--RTS
    # arguments.
    if not auto_target_args:
        return result

    # Otherwise, handle target and board information.
    #
    # Remember that the testsuite determines the target from the machine that
    # hosts the testsuite and from its own --host/--build/--target arguments...
    #
    # If we have a specific target board specified with --board, use that:
    #
    #   --target=p55-elf --board=iSystem-5554
    #   --> gnatcov run --target=iSystem-5554
    #
    # Such board indications are intended for probe based targets.
    gnatemu_board = gnatemu_board_name(thistest.options.board)
    if gnatemu_board:
        targetarg = gnatemu_board

    # Otherwise, pass the target triplet indication, completed by a board
    # extension if we also have a target "machine":
    #
    #   --target=p55-elf,,p5566
    #   --> gnatcov run --target=powerpc-eabispe,p5566
    #
    # Such board extensions are intended to request the selection of a specific
    # board emulation by gnatemu.
    #
    # Besides, we now have a single package per host OS (GNU/Linux and Windows,
    # both 64bit). "gnatcov" needs to know whether it works on 32bit or 64bit
    # programs, assuming 64bit by default. This means that we must pass a
    # --target argument to "gnatcov" as soon as we work with a 32bit target.
    elif thistest.options.target or env.target.cpu.bits != 64:
        targetarg = env.target.triplet
        board_name = gnatemu_board_name(env.target.machine)
        if board_name and board_name != "unknown":
            targetarg += ",%s" % board_name

    else:
        targetarg = None

    if targetarg:
        result.append("--target=" + targetarg)

    # Handle --RTS
    #
    # We must pass a --RTS argument as soon as we use a non-default runtime
    # *and* we pass a project file (proper GPR loading can require the
    # runtime information).
    if project_handling_enabled and thistest.options.RTS:
        result.append("--RTS=" + thistest.options.RTS)

    return result


def cmdrun(
    cmd, for_pgm, inp=None, out=None, err=None, env=None, register_failure=True
):
    """
    Execute the command+args list in CMD, redirecting its input, output and
    error streams to INP, OUT and ERR when not None, respectively. If ENV is
    not None, pass it as the subprocess environment.

    FOR_PGM tells if this execution actually runs a user/test program.

    Stop with a FatalError if the execution status is not zero and
    REGISTER_FAILURE is True. If FOR_PGM is also True (in addition to
    REGISTER_FAILURE) also check the program's output for an occurrence
    of unhandled exception in cross configurations.

    In absence of fatal error, return the process descriptor.
    """

    # Setup a dictionary of Run input/output/error arguments for which a
    # non default value is requested.

    kwargs = {
        key: value
        for key, value in [
            ("input", inp),
            ("output", out),
            ("error", err),
            ("env", env),
        ]
        if value
    }

    p = run_and_log(cmd, timeout=thistest.options.timeout, **kwargs)

    # Check for FatalError conditions. Minimize the situations where we look
    # into the program's output as this is a central spot.

    if register_failure and p.status != 0:
        output = contents_of(out) if out else p.out
        thistest.stop(
            FatalError(
                '"%s"' % " ".join(cmd) + " exit in error",
                outfile=out,
                outstr=output,
            )
        )

    if register_failure and for_pgm and thistest.options.target:
        output = contents_of(out) if out else p.out
        thistest.stop_if(
            unhandled_exception_in(output),
            FatalError(
                '"%s"' % " ".join(cmd) + " raised an unhandled exception",
                outfile=out,
                outstr=output,
            ),
        )

    return p


def xcov(
    args,
    out=None,
    err=None,
    inp=None,
    env=None,
    register_failure=True,
    auto_config_args=True,
    auto_target_args=True,
    force_project_args=False,
    auto_languages=True,
):
    """
    Run xcov with arguments ARGS, timeout control, valgrind control if
    available and enabled, output directed to OUT and failure registration if
    register_failure is True. Return the process status descriptor. ARGS may be
    a list or a whitespace separated strings.

    If AUTO_LANGUAGES is True, the gnatcov sub-command is "instrument" and the
    testsuite is not in qualification mode, automatically pass the
    --restricted-to-languages argument to enable all the languages to test.

    See xcov_suite_args for the meaning of AUTO_*_ARGS and FORCE_PROJECT_ARGS
    arguments.
    """

    # Defensive code: running "gnatcov setup" with no prefix will install
    # gnatcov_rts in gprbuild's prefix, i.e. the current test will try to
    # modify a global resource. This is always an error, so reject it before it
    # causes hard to trace damage elsewhere.
    assert args[0] != "setup" or any(
        arg.startswith("--prefix") for arg in args
    )

    # Make ARGS a list from whatever it is, to allow unified processing.
    # Then fetch the requested command, always first:
    args = to_list(args)
    covcmd = args[0]
    covargs = args[1:]

    if thistest.options.all_warnings:
        covargs = ["--all-warnings"] + covargs

    covargs = (
        xcov_suite_args(
            covcmd,
            covargs,
            auto_config_args,
            auto_target_args,
            force_project_args,
        )
        + covargs
    )

    # Determine which program we are actually going launch. This is
    # "gnatcov <cmd>" unless we are to execute some designated program
    # for this:
    covpgm = thistest.suite_covpgm_for(covcmd)
    covpgm = (
        [covpgm] if covpgm is not None else maybe_valgrind([XCOV]) + [covcmd]
    )

    # Execute, check status, raise on error and return otherwise.
    #
    # The gprvar options are only needed for the "libsupport" part of our
    # projects. They are pointless wrt coverage run or analysis activities
    # so we don't include them here.
    p = cmdrun(
        cmd=covpgm + covargs,
        inp=inp,
        out=out,
        err=err,
        env=env,
        register_failure=register_failure,
        for_pgm=(covcmd == "run"),
    )

    if thistest.options.enable_valgrind == "memcheck":
        memcheck_log = contents_of(MEMCHECK_LOG)
        thistest.fail_if(
            memcheck_log,
            "MEMCHECK log not empty"
            '\nFROM "%s":'
            "\n%s" % (" ".join(covpgm + covargs), memcheck_log),
        )

    return p


def xrun(
    args,
    out=None,
    env=None,
    register_failure=True,
    auto_config_args=True,
    auto_target_args=True,
):
    """
    Run <xcov run> with arguments ARGS for the current target, performing
    operations only relevant to invocations intended to execute a program (for
    example, requesting a limit on the output trace size).
    """

    # Force a dummy input to prevent mysterious qemu misbehavior when input is
    # a terminal.
    nulinput = "devnul"
    touch(nulinput)

    # Then possibly augment the arguments to pass.
    #
    # --kernel on the testsuite command line translates as --kernel to
    # gnatcov run.
    #
    # --trace-size-limit on the testsuite command line adds to the -eargs
    # passed to gnatcov run for cross targets running with an emulator.
    #
    # Be careful that we might have -eargs at the end of the input arguments
    # we receive.
    runargs = []

    if thistest.options.kernel:
        runargs.append("--kernel=" + thistest.options.kernel)

    runargs.extend(to_list(args))

    if (
        thistest.options.trace_size_limit
        and thistest.options.target
        and not gnatemu_board_name(thistest.options.board)
    ):
        if "-eargs" not in runargs:
            runargs.append("-eargs")
        runargs.extend(
            ["-exec-trace-limit", thistest.options.trace_size_limit]
        )

    return xcov(
        ["run"] + runargs,
        inp=nulinput,
        out=out,
        env=env,
        register_failure=register_failure,
        auto_config_args=auto_config_args,
        auto_target_args=auto_target_args,
    )


def run_cov_program(
    executable, out=None, env=None, exec_args=None, register_failure=True
):
    """
    Assuming that `executable` was instrumented, run it according to the
    current target.
    """

    args = []
    exec_args = exec_args or []
    inp = None
    use_pycross = False
    if thistest.options.target:
        # If we are testing for AAMP, use the facade simulator. It expects a
        # configuration file (facade.cfg) in the executable dir, and the
        # executable must be run through an executable.sod file, which sets
        # up the simulator environment. This .sod file should be in the same
        # directory as the executable.
        if "aamp" in control.env.target.platform:
            args.append("dosfsod.exe")
            list_to_file(
                [
                    "sw tx on",
                    '$TEXTIO = ""',
                    "switch batch on",
                    "fill 0000..ffff 0",
                    "load " + executable,
                    "go",
                    "halt",
                ],
                "test.sod",
            )
            args.append("@test.sod")
            cp(os.path.join(ROOT_DIR, "facade.cfg"), "facade.cfg")
            args.extend(exec_args)
            out = cmdrun(
                args,
                out=out,
                env=env,
                register_failure=register_failure,
                for_pgm=True,
            )
            return out

    # If we are in a cross configuration, run the program using run-cross2
    if thistest.options.target and thistest.env.target.platform != "c":
        use_pycross = True

        # We absolutely need a machine name to run programs with run-cross2
        assert thistest.options.board or thistest.env.target.machine

        target = thistest.env.target.platform
        os_ver = (
            thistest.env.target.os.version if thistest.env.target.os else ""
        )

        target += f",{os_ver}"

        if thistest.options.board:
            target += f",{thistest.options.board}"
        else:
            target += f",{thistest.env.target.machine}"

        # run-cross2 is a python script. As Windows does not parse shebangs,
        # use the python executable as main program instead of the script.
        args += [
            sys.executable,
            which("run-cross2"),
            f"--target={target}",
            # Instruct pycross to preserve temporary files, which includes the
            # directory that is mounted in the target's filesystem. This is
            # where the source trace may be created (see below).
            "--save-temps",
        ]
    else:
        # Native programs using a light runtime can't set the exit code, and
        # will often terminate with a non-zero status code even though nothing
        # went wrong. There is thus no point in checking the exit code in this
        # configuration.
        register_failure = (
            register_failure and not RUNTIME_INFO.has_light_runtime
        )

    args.append(executable)
    args.extend(exec_args)
    result = cmdrun(
        args,
        out=out,
        inp=inp,
        env=env,
        register_failure=register_failure,
        for_pgm=True,
    )

    # If the program was run under pycross, hoist source traces that the
    # program may have created on the target filesystem so that the rest of the
    # testsuite finds sources traces where they expect: in the current
    # directory.
    if use_pycross:
        for filename in glob.glob("hostfs-*/test/*.srctrace"):
            cp(filename, ".")

    return result


def do(command):
    """
    Execute COMMAND. Abort and dump output on failure. Return output otherwise.
    """
    p = cmdrun(cmd=to_list(command), register_failure=True, for_pgm=False)
    return p.out


class frame:
    def register(self, text):
        if len(text) > self.width:
            self.width = len(text)

    def display(self):
        thistest.log("\n" * self.pre + self.char * (self.width + 6))
        for text in self.lines:
            thistest.log(
                "%s %s %s"
                % (self.char * 2, text.center(self.width), self.char * 2)
            )
        thistest.log(self.char * (self.width + 6) + "\n" * self.post)

    def __init__(self, text, char="o", pre=1, post=1):
        self.pre = pre
        self.post = post
        self.char = char

        self.width = 0
        self.lines = text.split("\n")
        for text in self.lines:
            self.register(text)


@dataclass
class Ext_Annotation:
    """Base external annotation"""

    # File to which the annotation applies
    source_file: str

    # Start location for the annotation
    start_sloc: str

    # End location for the annotation, if relevant
    end_sloc: str | None

    def __init__(self, start_sloc, source_file, end_sloc=None):
        self.start_sloc = start_sloc
        self.source_file = source_file
        self.end_sloc = end_sloc

        # Use the local directory separator if we are dealing with a relative
        # source file path.
        if not os.path.isabs(self.source_file):
            self.source_file = self.source_file.replace("/", os.path.sep)
            self.source_file = self.source_file.replace("\\", os.path.sep)

    def cmd_line_args(self):
        args = [self.source_file]
        args.append(f"--kind={self.__class__.__name__}")
        if self.end_sloc:
            args.append(f"--start-location={self.start_sloc}")
            args.append(f"--end-location={self.end_sloc}")
        else:
            args.append(f"--location={self.start_sloc}")
        return args


@dataclass
class Exempt_On(Ext_Annotation):
    """Represents an external Exempt_On annotation"""

    # Justification for the exempted region
    justification: str | None = None

    def cmd_line_args(self):
        return super().cmd_line_args() + (
            [f"--justification={self.justification}"]
            if self.justification
            else []
        )


@dataclass
class Exempt_Region(Exempt_On):
    """
    Represents an external Exempt_Region annotation

    Requires end_sloc to be specified
    """

    def __post_init__(self):
        assert self.end_sloc


@dataclass
class Exempt_Off(Ext_Annotation):
    """Represents an external Exempt_Off annotation"""

    pass


@dataclass
class Cov_Off(Exempt_On):
    """Represents an external Cov_Off annotation"""

    pass


@dataclass
class Cov_On(Ext_Annotation):
    """Represents an external Cov_On annotation"""

    pass


@dataclass
class Reset_Buffers(Ext_Annotation):
    """Represents an external Reset_Buffer annotation"""

    # Whether the annotation should be inserted before or after the
    # statement designated by start_sloc
    insert_after: bool = False

    def cmd_line_args(self):
        return super().cmd_line_args() + (
            ["--annotate-after"] if self.insert_after else []
        )


@dataclass
class Dump_Buffers(Reset_Buffers):
    """Represents an external Dump_Buffers annotation"""

    # Optionnal trace prefix to be used when dumping the coverage buffers
    trace_prefix: str | None = None

    def cmd_line_args(self):
        return super().cmd_line_args() + (
            [f"--dump-filename-prefix={self.trace_prefix}"]
            if self.trace_prefix
            else []
        )


def xcov_annotate(
    annotation: Ext_Annotation,
    annot_out_file: str,
    annot_in_files: list[str] | None = None,
    extra_args: list[str] | None = None,
    out=None,
    env=None,
    register_failure=True,
    auto_config_args=True,
    auto_target_args=True,
):
    """
    Invoke "gnatcov annotate" with the correct arguments to generate
    an external annotation.

    No validation is performed on the function arguments before being
    transformed to a command line.

    :param Ext_Annotation) annotation: Annotation to be generated by gnatcov
    :param str source_file: source file to be annotated
    :param str annot_out_file: File to which the annotations should be written.
        This will overwrite any pre-existing file.
    :param list[str] | None annot_in_files : List of filenames containing
        pre-existing annotations, to be loaded and to which the new annotation
        will be added. Defaults to None.
    :param list[str] | None  extra_args: extra arguments passed on the command
        line. Defaults to None.
    """
    args = ["add-annotation"]
    args.extend(annotation.cmd_line_args())
    args.append(f"--output={annot_out_file}")
    if annot_in_files:
        args.extend(
            [f"--external-annotations={file}" for file in annot_in_files]
        )
    if extra_args:
        args.extend(extra_args)

    xcov(
        args,
        out=out,
        env=env,
        register_failure=register_failure,
        auto_config_args=auto_config_args,
        auto_target_args=auto_target_args,
    )


def generate_annotations(annotations, subdir=""):
    """
    Setup a temporary working directory in which an annotation file
    will be generated from annotations, using gnatcov add-annotation
    invocations.
    Returns the absolute path to the annotation file

    :param list[Ext_Annotation] annotations: List of annotation to be generated
        through the gnatcov add-annotation command
    """
    # Create a common temporary working dir in which the annotations will be
    # generated.
    tmp = Wdir(subdir if subdir else "tmp_annotations")

    # Create an empty annotation file, to simplify command line generation
    annot_file = os.path.abspath("annotations.toml")
    touch(annot_file)

    # Move to the home dir so that annotations are relative to the home
    # directory of the test.
    tmp.to_homedir()

    # Generate the annotations
    for annotation in annotations:
        xcov_annotate(
            annotation, annot_in_files=[annot_file], annot_out_file=annot_file
        )
    return annot_file
