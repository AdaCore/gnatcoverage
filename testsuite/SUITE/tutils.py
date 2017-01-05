# ***************************************************************************
# **                      TEST-COMMON UTILITY functions                    **
# ***************************************************************************

# This module exposes common utility functions to every test instance.  They
# depend on the current test context and are not suitable for the toplevel
# suite driver.

# ***************************************************************************

# Expose a few other items as a test util-facilities as well

from SUITE import control
from SUITE.control import LANGINFO, language_info, KNOWN_LANGUAGES
from SUITE.control import BUILDER, need_libsupport, xcov_pgm
from SUITE.context import *

# Then mind our own buisness

from SUITE.cutils import *
from gnatpython.fileutils import unixpath


XCOV     = xcov_pgm(thistest.options.auto_arch)
VALGRIND = 'valgrind' + env.host.os.exeext

MEMCHECK_LOG = 'memcheck.log'
CALLGRIND_LOG = 'callgrind-{}.log'

# -------------------------
# -- gprbuild_gargs_with --
# -------------------------

def gprbuild_gargs_with (thisgargs):
    """Compute and return all the toplevel gprbuild arguments to pass. Account
       for specific requests in THISGARGS."""

    # Force a few bits useful for practical reasons and without influence on
    # code generation, then add our testsuite configuration options (selecting
    # target model and board essentially).

    return [
        '-f',               # always rebuild
        '-XSTYLE_CHECKS=',  # style checks off
        '-p'                # create missing directories (obj, typically)
        ] + (
        thistest.gprconfoptions
        + thistest.gprvaroptions
        + to_list (thisgargs)
        )

# -------------------------
# -- gprbuild_cargs_with --
# -------------------------

def gprbuild_cargs_with (thiscargs, suitecargs=True):
    """Compute and return all the cargs arguments to pass on gprbuild
       invocations, including language agnostic and language specific ones
       (-cargs and -cargs:<lang>) when SUITECARGS is true. Account for
       specific requests in THISCARGS."""

    # To make sure we have a clear view of what options are used for a
    # qualification run, qualification tests are not allowed to state
    # compilation flags of their own

    thistest.stop_if (
        thistest.options.qualif_level and (not suitecargs or thiscargs),
        FatalError("CARGS requested for qualification test. Forbidden."))

    # Compute the language specific cargs, all testsuite level:

    lang_cargs = []

    def add_for_lang(lang):
        lcargs = to_list (thistest.suite_cargs_for (lang))
        if lcargs:
            lang_cargs.extend (["-cargs:%s" % lang] + lcargs)

    if suitecargs:
        [add_for_lang (lang=lang) for lang in KNOWN_LANGUAGES]

    # Compute the language agnostic cargs, testsuite level + those
    # for this specific run:

    other_cargs = []

    if suitecargs:
        other_cargs.extend (to_list(thistest.suite_cargs_for (lang=None)))

    other_cargs.extend (to_list (thiscargs))

    if other_cargs:
        other_cargs = ['-cargs'] + other_cargs

    return lang_cargs + other_cargs

# -------------------------
# -- gprbuild_largs_with --
# -------------------------

def gprbuild_largs_with (thislargs):
    """Compute and return all the largs gprbuild arguments to pass.
       Account for specific requests in THISLARGS."""

    all_largs = to_list (thistest.options.largs)
    all_largs.extend (to_list (thislargs))

    if all_largs:
        all_largs.insert (0, '-largs')

    return all_largs

# --------------
# -- gprbuild --
# --------------

def gprbuild(
    project, extracargs=None, gargs=None, largs=None, suitecargs=True):

    """Cleanup & build the provided PROJECT file using gprbuild, passing
    GARGS/CARGS/LARGS as gprbuild/cargs/largs command-line switches, in
    addition to the switches required by the infrastructure or provided on the
    testsuite commandline for the --cargs family when SUITECARGS is true.

    The *ARGS arguments may be either: None, a string containing
    a space-separated list of options, or a list of options."""

    # Fetch options, from what is requested specifically here
    # or from command line requests

    all_gargs = gprbuild_gargs_with (thisgargs=gargs)
    all_largs = gprbuild_largs_with (thislargs=largs)
    all_cargs = gprbuild_cargs_with (
        thiscargs=extracargs, suitecargs=suitecargs)

    # Now cleanup, do build and check status

    thistest.cleanup(project)

    ofile = "gprbuild.out"
    p = Run (
        to_list(BUILDER.BASE_COMMAND) + ['-P%s' % project]
        + all_gargs + all_cargs + all_largs,
        output=ofile, timeout=thistest.options.timeout
        )
    thistest.stop_if (
        p.status != 0, FatalError("gprbuild exit in error", ofile))

# ------------
# -- gprfor --
# ------------
def gprfor(
    mains, prjid="gen", srcdirs="src", objdir=None, exedir=".",
    main_cargs=None, langs=None, deps=(), compiler_extra="", extra=""
    ):
    """Generate a simple PRJID.gpr project file to build executables for each
    main source file in the MAINS list, sources in SRCDIRS. Inexistant
    directories in SRCDIRS are ignored. Assume the set of languages is LANGS
    when specified; infer from the mains otherwise. Add COMPILER_EXTRA, if any,
    at the end of the Compiler package contents. Add EXTRA, if any, at the
    end of the project file contents. Return the gpr file name.
    """

    deps = '\n'.join (
        ["with \"%s\";" % dep for dep in deps])

    mains = to_list(mains)
    srcdirs = to_list(srcdirs)
    langs = to_list(langs)

    # Fetch the support project file template
    template = contents_of (os.path.join (ROOT_DIR, "template.gpr"))

    # Instanciate the template fields.

    # Turn the list of main sources into the proper comma separated sequence
    # of string literals for the Main GPR attribute.

    gprmains = ', '.join(['"%s"' % m for m in mains])

    # Likewise for source dirs. Filter on existence, to allow widening the set
    # of tentative dirs while preventing complaints from gprbuild about
    # inexistent ones. Remove a lone trailing comma, which happens when none
    # of the provided dirs exists and would produce an invalid gpr file.

    srcdirs = ', '.join(['"%s"' % d for d in srcdirs if os.path.exists(d)])
    srcdirs = srcdirs.rstrip(', ')

    # Determine the language(s) from the mains.

    languages_l = langs or set(
        [language_info(main).name for main in mains]
        )

    languages = ', '.join(['"%s"' %l for l in languages_l])

    # The base project file we need to extend, and the way to refer to it
    # from the project contents. This provides a default last chance handler
    # on which we rely to detect termination on exception occurrence.

    basegpr = (
        ("%s/support/base" % ROOT_DIR) if control.need_libsupport ()
        else None)

    baseref = (
        (basegpr.split('/')[-1] + ".") if basegpr else "")

    # Generate compilation switches:
    #
    # - For each language, add BUILDER.COMMON_CARGS as default switches.
    #
    # - If we have specific flags for the mains, append them. This is
    #   typically something like:
    #
    #    for Switches("test_blob.adb") use
    #      Compiler'Default_Switches("Ada") & ("-fno-inline")

    default_switches = ', '.join(
        ['"%s"' % switch for switch in BUILDER.COMMON_CARGS()]
        )
    compswitches = (
        '\n'.join (
            ['for Default_Switches ("%s") use (%s);' % (
                    language, default_switches)
             for language in languages_l]) + '\n' +
        '\n'.join (
            ['for Switches("%s") use \n'
             '  Compiler\'Default_Switches ("%s") & (%s);' % (
                    main, language_info(main).name, ','.join(
                        ['"%s"' % carg for carg in to_list(main_cargs)]))
             for main in mains]
            ) + '\n'
        )

    # Now instanciate, dump the contents into the target gpr file and return

    gprtext = template % {
        'prjname': prjid,
        'extends': ('extends "%s"' % basegpr) if basegpr else "",
        'srcdirs': srcdirs,
        'exedir': exedir,
        'objdir': objdir or (exedir+"/obj"),
        'compswitches': compswitches,
        'languages' : languages,
        'gprmains': gprmains,
        'deps': deps,
        'compiler_extra': compiler_extra,
        'extra': extra}

    return text_to_file (text = gprtext, filename = prjid + ".gpr")

# ------------------------------------------------------------
# -- exename_for, tracename_for, dmapname_for, ckptname_for --
# ------------------------------------------------------------

# Abstract away the possible presence of extensions at the end of executable
# names depending on the target, e.g. ".out" for vxworks.

# PGNNAME is a program name, in the main subprogram name sense. An empty
# PGMNAME is allowed, in which case the functions return only the extensions.

# Executable name

def exename_for (pgmname):
    return (pgmname + thistest.tinfo.exeext) if thistest.tinfo else pgmname

# Tracefile name

def tracename_for (pgmname):
    return exename_for (pgmname) + ".trace"

# Decision map name

def dmapname_for (pgmname):
    return exename_for (pgmname) + ".dmap"

# Coverage checkpoint name

def ckptname_for (pgmname):
    return exename_for (pgmname) + ".ckpt"

# -----------------------------
# -- exepath_to, unixpath_to --
# -----------------------------

# Those two are very similar. The unix version is mostly useful on Windows for
# tests that are going to search for exe filenames in outputs using regular
# expressions, where backslashes as directory separators introduce confusion.

def exepath_to (pgmname):
    """Return the absolute path to the executable file expected
    in the current directory for a main subprogram PGMNAME."""

    return os.path.abspath(exename_for(pgmname))

def unixpath_to (pgmname):
    """Return the absolute path to the executable file expected in the
    current directory for a main subprogram PGMNAME, unixified."""

    return unixpath(os.path.abspath(exename_for(pgmname)))

# --------------------
# -- maybe_valgrind --
# --------------------
def maybe_valgrind(command):
    """Return the input COMMAND list, wrapped with valgrind or callgrind,
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

# -------------------------------
# -- platform_specific_symbols --
# -------------------------------
def platform_specific_symbols(symbols):
    """
    Given SYMBOLS, a list of architecture-independant symbol names, return a
    list of corresponding of architecture-specific symbol names.

    For instance for Windows, this prepends an underscore to every symbol name.
    """
    return [thistest.tinfo.to_platform_specific_symbol(sym) for sym in symbols]

# ----------
# -- xcov --
# ----------
def xcov(args, out=None, err=None, inp=None, register_failure=True,
        auto_target_arg=True):
    """Run xcov with arguments ARGS, timeout control, valgrind control if
    available and enabled, output directed to OUT and failure registration
    if register_failure is True. Return the process status descriptor. ARGS
    may be a list or a whitespace separated string.

    If AUTO_TARGET_ARG, automatically add a "--target" argument in cross
    configurations."""

    # Make ARGS a list from whatever it is, to allow unified processing.
    # Then fetch the requested command, always first:

    args = to_list (args)
    covcmd = args[0]
    covargs = args[1:]

    if thistest.options.trace_dir is not None:
        # Bootstrap - run xcov under xcov

        if covcmd == 'coverage':
            thistest.current_test_index += 1
            args = ['run', '-t', 'i686-pc-linux-gnu',
                    '-o', os.path.join(thistest.options.trace_dir,
                                       str(thistest.current_test_index)
                                       + '.trace'),
                    which(XCOV), '-eargs'] + args

    # Handle --target and --board

    # We must pass a --target argument if we are in a cross configuration *and*
    # if either 1) this is the "run" command (so that the proper GNATemulator
    # is run), or 2) we pass a project file (proper GPR loading can require the
    # target).
    #
    # If we have a specific target board specified with --board, use that:
    #
    # --target=p55-elf --board=iSystem-5554
    # --> gnatcov run --target=iSystem-5554
    #
    # (Such board indications are intended for probe based targets)
    #
    # Otherwise, pass the target triplet indication, completed by a board
    # extension if we also have a target "machine":
    #
    # --target=p55-elf,,p5566
    # --> gnatcov run --target=powerpc-eabispe,p5566
    #
    # (Such board extensions are intended to request the selection of a
    #  specific board emulation by gnatemu)

    if auto_target_arg and (
        covcmd == 'run' or any(arg.startswith('-P') for arg in covargs)
    ):
        if thistest.options.board:
            targetarg = thistest.options.board
        elif thistest.options.target:
            targetarg = env.target.triplet
            if env.target.machine and env.target.machine != "unknown":
                targetarg += ",%s" % env.target.machine
        else:
            targetarg = None

        if targetarg:
            covargs.insert(0, '--target=' + targetarg)

    # Determine which program we are actually going launch. This is
    # "gnatcov <cmd>" unless we are to execute some designated program
    # for this:

    covpgm = thistest.suite_covpgm_for (covcmd)
    covpgm = (
        [covpgm] if covpgm is not None
        else maybe_valgrind([XCOV]) + [covcmd]
        )

    # Execute, check status, raise on error and return otherwise.

    # The gprvar options are only needed for the "libsupport" part of our
    # projects. They are pointless wrt coverage run or analysis activities
    # so we don't include them here.

    # If input(inp)/output(out)/error(err) are not given, we want to use Run
    # defaults values: do not add them to kwargs if they are None.

    kwargs = {}
    [kwargs.__setitem__(key, value)
     for (key, value) in  (
            ('input', inp),
            ('output', out),
            ('error', err))
     if value]

    p = Run(covpgm + covargs, timeout=thistest.options.timeout, **kwargs)

    if thistest.options.enable_valgrind == 'memcheck':
        memcheck_log = contents_of (MEMCHECK_LOG)
        thistest.fail_if(
            memcheck_log,
            FatalError(
                'MEMCHECK log not empty\n'
                + 'FROM "%s":\n%s' % (
                    ' '.join(covpgm + covargs), memcheck_log)))

    thistest.stop_if(
        register_failure and p.status != 0,
        FatalError(
            '"%s"' % ' '.join(covpgm + covargs) + ' exit in error',
            outfile = out, outstr = p.out))

    return p

# ---------------------
# -- xrun_suite_args --
# ---------------------
def xrun_suite_args():
    """Arguments we should pass to gnatcov run to obey what we received on the
    testsuite command line, in particular --kernel."""

    # --kernel translates as --kernel to gnatcov run.
    return (['--kernel=' + thistest.options.kernel]
            if thistest.options.kernel else [])

# ----------
# -- xrun --
# ----------
def xrun(args, out=None, register_failure=True, auto_target_arg=True):
    """Run <xcov run> with arguments ARGS for the current target."""

    # We special case xcov run to pass the extra option corresponding to the
    # --kernel request on our command line, and to force a dummy input to
    # prevent mysterious qemu misbehavior when input is a terminal.

    nulinput = "devnul"
    touch(nulinput)

    runargs = xrun_suite_args()
    runargs.extend (to_list(args))

    return xcov (
        ['run'] + runargs, inp=nulinput, out=out,
        register_failure=register_failure,
        auto_target_arg=auto_target_arg)

# --------
# -- do --
# --------
def do(command):
    """Execute COMMAND. Abort and dump output on failure. Return output
    otherwise."""

    ofile = "cmd_.out"
    p = Run(to_list (command), output=ofile)

    thistest.stop_if(p.status != 0,
        FatalError("command '%s' failed" % command, ofile))

    return contents_of(ofile)

# -----------
# -- frame --
# -----------
class frame:

    def register(self, text):
        if len(text) > self.width:
            self.width = len(text)

    def display(self):
        thistest.log('\n' * self.pre + self.char * (self.width + 6))
        [thistest.log(
            "%s %s %s" % (
            self.char * 2, text.center(self.width), self.char*2))
         for text in self.lines]
        thistest.log(self.char * (self.width + 6) + '\n' * self.post)

    def __init__(self, text, char='o', pre=1, post=1):
        self.pre  = pre
        self.post = post
        self.char = char

        self.width = 0
        self.lines = text.split('\n')
        [self.register(text) for text in self.lines]

