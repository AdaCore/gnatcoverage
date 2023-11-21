# ***************************************************************************
# **                      TEST-COMMON UTILITY functions                    **
# ***************************************************************************

# This module exposes common utility functions to every test instance.  They
# depend on the current test context and are not suitable for the toplevel
# suite driver.

# ***************************************************************************

import glob
import os
import re
import time


from e3.os.fs import touch, unixpath
from e3.os.process import DEVNULL, Run


# Expose a few other items as a test util facilities as well

from SUITE import control
from SUITE.control import (BUILDER, KNOWN_LANGUAGES, env, language_info,
                           xcov_pgm)
from SUITE.context import ROOT_DIR, thistest


# Then mind our own business

from SUITE.cutils import (FatalError, contents_of, text_to_file, to_list,
                          unhandled_exception_in)


# Precompute some values we might be using repeatedly

TARGET_INFO = control.target_info()
RUNTIME_INFO = control.runtime_info()
GNATCOV_INFO = control.gnatcov_info()

XCOV = xcov_pgm(thistest.options.auto_arch)
VALGRIND = 'valgrind' + env.host.os.exeext

MEMCHECK_LOG = 'memcheck.log'
CALLGRIND_LOG = 'callgrind-{}.log'

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
        cmd = kwargs['cmds']
    except KeyError:
        cmd = args[0]
    p.original_cmd = cmd
    p.duration = time.time() - start
    run_processes.append(p)

    return p


def gprbuild_gargs_with(thisgargs,
                        trace_mode=None,
                        runtime_project=None):
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
        '-f',               # always rebuild
        '-XSTYLE_CHECKS=',  # style checks off
        '-p'                # create missing directories (obj, typically)
    ]

    # Add our testsuite configuration options (selecting target model and board
    # essentially).
    result.extend(thistest.gprconfoptions)
    result.extend(thistest.gprvaroptions)
    result.extend(to_list(thisgargs))

    # If we work with source instrumentation, add the dependency on the
    # instrumentation runtime project so that instrumented programs are
    # compilable in the generated projects. Also use instrumented sources in
    # the "*-gnatcov-instr" object directories.
    if trace_mode == 'src':
        runtime_project = (
            runtime_project or RUNTIME_INFO.gnatcov_rts_project
        )
        result += [
            f"--implicit-with={runtime_project}.gpr",
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
            FatalError("SCOV_CARGS required for qualification test"))
        thistest.stop_if(
            thiscargs,
            FatalError("Specific CARGS forbidden for qualification test"))

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
        other_cargs = ['-cargs'] + other_cargs

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
    if (thistest.env.build.os.name == 'windows'
            and thistest.options.trace_mode == 'bin'):
        all_largs.append('-no-pie')

    if all_largs:
        all_largs.insert(0, '-largs')

    return all_largs


def gprbuild(project,
             scovcargs=True,
             suitecargs=True,
             extracargs=None,
             gargs=None,
             largs=None,
             trace_mode=None,
             runtime_project=None,
             out='gprbuild.out',
             register_failure=True):
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
    """

    # Fetch options, from what is requested specifically here
    # or from command line requests
    all_gargs = gprbuild_gargs_with(
        thisgargs=gargs,
        trace_mode=trace_mode,
        runtime_project=runtime_project,
    )
    all_largs = gprbuild_largs_with(thislargs=largs)
    all_cargs = gprbuild_cargs_with(scovcargs=scovcargs,
                                    suitecargs=suitecargs,
                                    thiscargs=extracargs)

    # Now cleanup, do build and check status
    thistest.cleanup(project)

    args = (to_list(BUILDER.BASE_COMMAND) +
            ['-P%s' % project] + all_gargs + all_cargs + all_largs)
    p = run_and_log(args, output=out, timeout=thistest.options.timeout)
    if register_failure:
        thistest.stop_if(p.status != 0,
                         FatalError("gprbuild exit in error", out))
    return p


def gprinstall(project, gargs=None):
    """
    Run "gprinstall" on the provided project file.

    :param None|list[str] gargs: list of command line switches to pass to
        gprinstall
    """
    ofile = 'gprinstall.out'
    args = ['gprinstall', '-P', project, '-p']

    # Add mandatory options, such as target and RTS info
    args.extend(thistest.gprconfoptions)
    args.extend(thistest.gprvaroptions)

    # Add user-provided arguments
    args.extend(to_list(gargs))

    p = run_and_log(args, output=ofile, timeout=thistest.options.timeout)
    thistest.stop_if(p.status != 0,
                     FatalError('gprinstall exit in error', ofile))


def gpr_emulator_package():
    """
    If there is a board name, return a package Emulator to be included in a GPR
    file to provide this information to GNATemulator.
    """
    return ('package Emulator is\n'
            '   for Board use "{}";\n'
            'end Emulator;'.format(env.target.machine)
            if env.target.machine else '')


def gprfor(mains, prjid="gen", srcdirs="src", objdir=None, exedir=".",
           main_cargs=None, langs=None, deps=None, scenario_extra="",
           compiler_extra="", extra="", cwd=None):
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
    gprmains = ', '.join('"%s"' % m for m in mains)
    if gprmains:
        gprmains = 'for Main use (%s);' % gprmains

    # Likewise for source dirs. Filter on existence, to allow widening the set
    # of tentative dirs while preventing complaints from gprbuild about
    # inexistent ones.
    srcdirs_list = [
        d
        for d in srcdirs
        if os.path.exists(os.path.join(cwd, d))
    ]

    # Determine the language(s) from the sources if they are not explicitly
    # passed as parameters.
    if not langs:
        lang_infos = [language_info(src)
                      for srcdir in srcdirs_list
                      for src in os.listdir(os.path.join(cwd, srcdir))]
        langs = set(li.name for li in lang_infos if li)

    srcdirs = ', '.join('"%s"' % d for d in srcdirs_list)
    languages = ', '.join('"%s"' % lang for lang in langs)

    # In addition to the provided dependencies, figure out if this project
    # should extend or with some support or helper facilities. These are
    # designed with projects for test *programs* in mind, not for libraries,
    # and would actually be plain incompatible with shared Library projects.
    for_library = "Library" in extra

    # The base project file we need to extend, which drags libsupport,
    # and the way to refer to it from the project contents.
    basegpr = (
        "{}/support/base.gpr".format(ROOT_DIR) if not for_library else None)

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

    deps = '\n'.join('with "%s";' % dep for dep in deps)

    # If we have specific flags for the mains, append them. This is
    # typically something like:
    #
    #    for Switches("test_blob.adb") use
    #      Compiler'Default_Switches("Ada") & ("-fno-inline")

    compswitches = (
        '\n'.join(
            ['for Switches("%s") use \n'
             '  Compiler\'Default_Switches ("%s") & (%s);' % (
                    main, language_info(main).name, ','.join(
                        ['"%s"' % carg for carg in to_list(main_cargs)]))
             for main in mains]
            ) + '\n')

    # Now instanciate, dump the contents into the target gpr file and return
    gprtext = template % {
        'prjname': prjid,
        'extends': ('extends "%s"' % basegpr) if basegpr else "",
        'scenario': scenario_extra,
        'srcdirs': srcdirs,
        'exedir': exedir,
        'objdir': objdir or (exedir + "/obj"),
        'compswitches': compswitches,
        'languages': languages,
        'gprmains': gprmains,
        'deps': deps,
        'compiler_extra': compiler_extra,
        'pkg_emulator': gpr_emulator_package(),
        'extra': extra}

    return text_to_file(
        text=gprtext, filename=os.path.join(cwd, prjid + ".gpr")
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


def srctracename_for(pgmname, register_failure=True, manual=False,
                     manual_prj_name=None):
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
    elif thistest.options.enable_valgrind == 'memcheck':
        prefix = [VALGRIND, '-q', '--log-file=%s' % MEMCHECK_LOG]
    elif thistest.options.enable_valgrind == 'callgrind':
        log_file = CALLGRIND_LOG.format(thistest.create_callgrind_id())
        prefix = [
            VALGRIND, '-q', '--tool=callgrind',
            '--callgrind-out-file=%s' % log_file]
    else:
        raise ValueError('Invalid Valgrind tool: {}'.format(
            thistest.options.enable_valgrind))
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


def xcov_suite_args(covcmd, covargs,
                    auto_config_args=True,
                    auto_target_args=True,
                    force_project_args=False):
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
        force_project_args or any(arg.startswith('-P') for arg in covargs)
    )

    # If --config is asked and project handling is involved, pass it and stop
    # there. If there is a board, it must be described in the project file
    # (gnatcov's -P argument).
    if auto_config_args and project_handling_enabled:
        return ['--config={}'
                .format(os.path.join(ROOT_DIR, BUILDER.SUITE_CGPR))]

    # Nothing to do if the caller does not want automatic --target/--RTS
    # arguments.
    if not auto_target_args:
        return []

    # Otherwise, handle target and board information.
    #
    # Remember that the testsuite determines the target from the machine that
    # hosts the testsuite and from its own --host/--build/--target arguments...

    result = []

    # If we have a specific target board specified with --board, use that:
    #
    #   --target=p55-elf --board=iSystem-5554
    #   --> gnatcov run --target=iSystem-5554
    #
    # Such board indications are intended for probe based targets.
    if thistest.options.board:
        targetarg = thistest.options.board

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
        if env.target.machine and env.target.machine != "unknown":
            targetarg += ",%s" % env.target.machine

    else:
        targetarg = None

    if targetarg:
        result.append('--target=' + targetarg)

    # Handle --RTS
    #
    # We must pass a --RTS argument as soon as we use a non-default runtime
    # *and* we pass a project file (proper GPR loading can require the
    # runtime information).
    if project_handling_enabled and thistest.options.RTS:
        result.append('--RTS=' + thistest.options.RTS)

    return result


def cmdrun(cmd, for_pgm, inp=None, out=None, err=None, env=None,
           register_failure=True):
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
        for key, value in [('input', inp), ('output', out), ('error', err),
                           ('env', env)]
        if value
    }

    p = run_and_log(cmd, timeout=thistest.options.timeout, **kwargs)

    # Check for FataError conditions. Minimize the situations where we look
    # into the program's output as this is a central spot.

    if register_failure and p.status != 0:
        output = contents_of(out) if out else p.out
        thistest.stop(
            FatalError(
                '"%s"' % ' '.join(cmd) + ' exit in error',
                outfile=out, outstr=output)
        )

    if register_failure and for_pgm and thistest.options.target:
        output = contents_of(out) if out else p.out
        thistest.stop_if(
            unhandled_exception_in(output),
            FatalError(
                '"%s"' % ' '.join(cmd) + ' raised an unhandled exception',
                outfile=out, outstr=output)
        )

    return p


def xcov(args, out=None, err=None, inp=None, env=None, register_failure=True,
         auto_config_args=True, auto_target_args=True,
         force_project_args=False, auto_languages=True):
    """
    Run xcov with arguments ARGS, timeout control, valgrind control if
    available and enabled, output directed to OUT and failure registration if
    register_failure is True. Return the process status descriptor. ARGS may be
    a list or a whitespace separated string.

    If AUTO_LANGUAGES is True, the gnatcov sub-command is "instrument" and the
    testsuite is not in qualification mode, automatically pass the
    --restricted-to-languages argument to enable all the languages to test.

    See xcov_suite_args for the meaning of AUTO_*_ARGS and FORCE_PROJECT_ARGS
    arguments.
    """

    # Make ARGS a list from whatever it is, to allow unified processing.
    # Then fetch the requested command, always first:
    args = to_list(args)
    covcmd = args[0]
    covargs = args[1:]

    if thistest.options.all_warnings:
        covargs = ['--all-warnings'] + covargs

    covargs = xcov_suite_args(covcmd, covargs, auto_config_args,
                              auto_target_args, force_project_args) + covargs

    # Determine which program we are actually going launch. This is
    # "gnatcov <cmd>" unless we are to execute some designated program
    # for this:
    covpgm = thistest.suite_covpgm_for(covcmd)
    covpgm = ([covpgm] if covpgm is not None
              else maybe_valgrind([XCOV]) + [covcmd])

    # Execute, check status, raise on error and return otherwise.
    #
    # The gprvar options are only needed for the "libsupport" part of our
    # projects. They are pointless wrt coverage run or analysis activities
    # so we don't include them here.
    p = cmdrun(cmd=covpgm + covargs, inp=inp, out=out, err=err, env=env,
               register_failure=register_failure,
               for_pgm=(covcmd == "run"))

    if thistest.options.enable_valgrind == 'memcheck':
        memcheck_log = contents_of(MEMCHECK_LOG)
        thistest.fail_if(
            memcheck_log,
            'MEMCHECK log not empty'
            '\nFROM "%s":'
            '\n%s' % (' '.join(covpgm + covargs), memcheck_log))

    return p


def xrun(args, out=None, env=None, register_failure=True,
         auto_config_args=True, auto_target_args=True):
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
        runargs.append('--kernel=' + thistest.options.kernel)

    runargs.extend(to_list(args))

    if (
        thistest.options.trace_size_limit and
        thistest.options.target and
        not thistest.options.board
    ):
        if '-eargs' not in runargs:
            runargs.append('-eargs')
        runargs.extend(["-exec-trace-limit",
                        thistest.options.trace_size_limit])

    return xcov(['run'] + runargs, inp=nulinput, out=out, env=env,
                register_failure=register_failure,
                auto_config_args=auto_config_args,
                auto_target_args=auto_target_args)


def run_cov_program(executable, out=None, env=None, exec_args=None,
                    register_failure=True):
    """
    Assuming that `executable` was instrumented, run it according to the
    current target.
    """

    args = []
    exec_args = exec_args or []
    inp = None

    # If we are in a cross configuration, run the program using GNATemulator
    if thistest.options.target:
        kernel = thistest.options.kernel
        board = thistest.options.board or thistest.env.target.machine
        args.append('{}-gnatemu'.format(thistest.env.target.triplet))
        if kernel:
            args.append('--kernel=' + kernel)
        if board:
            args.append('--board=' + board)

        # If GNATemulator runs as a background process in an interactive shell,
        # the Linux kernel will send a SIGTTIN when GNATemulator tries to read
        # the standard input (which is the interactive shell by default). This
        # will result in an abnormal GNATemulator termination, and the test
        # failing. Redirecting the standard input to /dev/null works around
        # this issue.
        inp = DEVNULL
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
    return cmdrun(args, out=out, inp=inp, env=env,
                  register_failure=register_failure,
                  for_pgm=True)


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
        thistest.log('\n' * self.pre + self.char * (self.width + 6))
        for text in self.lines:
            thistest.log("%s %s %s" % (
                self.char * 2, text.center(self.width), self.char*2))
        thistest.log(self.char * (self.width + 6) + '\n' * self.post)

    def __init__(self, text, char='o', pre=1, post=1):
        self.pre = pre
        self.post = post
        self.char = char

        self.width = 0
        self.lines = text.split('\n')
        for text in self.lines:
            self.register(text)
