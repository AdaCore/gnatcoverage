"""
Testsuite control
"""

import os.path
import re

from e3.env import Env
from e3.fs import rm
from e3.os.process import Run

from SUITE.cutils import no_ext, version


env = Env()


def xcov_pgm(auto_arch, for_target=True):
    """Return the name of the "gnatcov" program to run.

    :param bool auto_arch: If True, autodetect which "gnatcov" run depending on
        FOR_TARGET.

    :param for_target: If True, consider that we run "gnatcov" for the target
        architecture. Otherwise, consider that we run it for the build
        architecture instead.
    """
    arch = env.target if for_target else env.build
    return 'gnatcov{bits}{ext}'.format(
        bits=str(arch.cpu.bits) if auto_arch else '',
        ext=env.host.os.exeext)


# Append .exe on windows for native tools
GPRBUILD = 'gprbuild' + env.host.os.exeext
GPRCONFIG = 'gprconfig' + env.host.os.exeext
GPRCLEAN = 'gprclean' + env.host.os.exeext


class LangInfo:
    """A class that provides some info about a given language.

    ATTRIBUTES:
        name:
          The name of the language. Eg: "Ada", or "C".

        src_ext:
           A list of extensions used for filenames of that language.
           Eg: [".ads", ".adb"] for GNAT, or [".h", ".c"] for C.

        comment:
           The comment marker used by that language to specify the start of a
           comment that runs until the end of the current line.  For instance,
           in Ada, it would be '--'.

        scofile_for:
           A function which returns the name of the file where SCOs can
           be found for a given SOURCE file name. This is, for example,
           "x.ali" for "x.adb" in Ada or "t.c.gli" for "t.c" in C.

        sidfile_for:
           A function which returns the name of the SID file for a given SOURCE
           file name. This is, for example, "x.sid" for "x.adb".
    """
    def __init__(self, name, src_ext, comment, scofile_for, sidfile_for=None):
        self.name = name
        self.src_ext = src_ext
        self.comment = comment
        self.scofile_for = scofile_for
        self.sidfile_for = sidfile_for


# A dictionary mapping a LangInfo instance to each known language.
# The set of recognized extensions is limited to those that gprbuild
# recognizes by default, on purpose to encourage consistency in the
# choice of filenames throughout the tests.

LANGINFO = {
    "Ada": LangInfo(
        name="Ada", src_ext=[".ads", ".adb"], comment='--',
        scofile_for=lambda source: no_ext(source) + '.ali',
        sidfile_for=lambda source: no_ext(source) + '.sid'),

    "C": LangInfo(
        name="C", src_ext=[".h", ".c"],  comment='//',
        scofile_for=lambda source: source + '.gli',
        sidfile_for=lambda source: source + '.sid'),

    "C++": LangInfo(
        name="C++", src_ext=[".hh", ".cpp"],
        comment='//',
        scofile_for=None,
        sidfile_for=lambda source: source + '.sid'),

    "Asm": LangInfo(
        name="Asm", src_ext=[".s"], comment='#',
        scofile_for=None),

    "Cons": LangInfo(
        name="Consolidation", src_ext=[".txt"], comment='--',
        scofile_for=None)}

# List of languages that gnatcov supports
KNOWN_LANGUAGES = [li.name for li in LANGINFO.values() if li.scofile_for]


def language_info(source_filename):
    """Return the LangInfo associated to a given source filename.
    The language is determined based on the filename extension.

    Return None if we couldn't determine the language.
    """
    ext = os.path.splitext(source_filename)[1]
    for lang_info in LANGINFO.values():
        if ext in lang_info.src_ext:
            return lang_info
    return None


class BUILDER:
    """
    Builder related testsuite controls, such as default compilation options
    or common GPR configuration setup. These are used to control the tests'
    execution and are displayed in qualification material.
    """

    @staticmethod
    def SCOV_CARGS(options):
        """
        Compilation args needed by tests exercising source coverage, passed by
        default to build invocations issued through the gprbuild() API.

        These depend on the (provided) toplevel testsuite command line
        options, indicating for example whether we do a run based on binary
        traces or on source instrumentation.
        """

        # Working from binary traces relies on specific properties:

        if options.trace_mode == 'bin':

            # Critical conditional branches must be preserved, source
            # coverage obligations need to be generated by the compiler,
            # and debug info is needed.

            cargs = ["-fpreserve-control-flow", "-fdump-scos", "-g"]

            # Proper support of inlining or generics requires advanced debug
            # info features possibly disabled by default on some targets.  We
            # could enforce this with an explicit option unconditionally, but
            # it is cleaner and simpler to have this exposed only when needed
            # in qualification material.

            # In addition, identification of call targets, necessary to
            # recognize exception edges, currently requires call-site debug
            # info on partially linked objects, which we'll only get for long
            # calls on powerpc.

            if 'vxworks' in Env().target.triplet:
                cargs.append("-gno-strict-dwarf")

                if 'powerpc' in Env().target.triplet:
                    cargs.append("-mlongcall")

            return cargs

        else:
            return []

    # Base command for a build
    BASE_COMMAND = GPRBUILD

    # Configuration file suitable for all the builder invocations,
    # setup early, once, to latch the compiler paths and RTS settings
    SUITE_CGPR = "suite.cgpr"

    @staticmethod
    def RUN_CONFIG_SEQUENCE(toplev_options, toolchain_discriminant):
        """Arrange to generate the SUITE_CONFIG configuration file"""

        # In principle, this would be something like
        #
        #  gprconfig --config=C --config=Asm --config=Ada --target=powerpc-elf
        #
        # to latch the compiler selections for all the languages, plus extra
        # bits for the RTS selection.
        #
        # RTS selection by relative path (e.g.
        #   --RTS=powerpc-elf/ravenscar-full-prep) isn't supported by
        # gprconfig, however. It is supported gprbuild though, so we resort
        # to it here.

        # We build a temporary dummy project file in the current directory,
        # specifying languages only.
        with open("suite.gpr", "w") as tempgpr:

            # Given GNAT 5.04a1 does not come with a C++ compiler, we'd
            # resort to the system one to link if C++ is in the list of
            # languages, causing compatibility issues with the toolchain.

            # We just don't add C++ to the list of languages for such
            # toolchain.

            added_languages = (
                ', "C++"' if toolchain_discriminant != "5.04a1"
                else '')

            tempgpr.write("""
                project %(prjname)s is
                   for Languages use ("Asm", "C", "Ada" %(added_languages)s);
                end %(prjname)s;
            """ % {'prjname': os.path.basename(tempgpr.name).split('.')[0],
                   'added_languages': added_languages})

        # We now run gprbuild -Ptemp.gpr --target=bla --RTS=blo, which
        # will complain about missing sources, but only after producing
        # an automatic config file with everything we need, and nothing
        # else (no other file).
        rm(BUILDER.SUITE_CGPR)

        extraopts = ['--target={}'.format(env.target.triplet)]
        if toplev_options.RTS:
            extraopts.append('--RTS=%s' % toplev_options.RTS)

        Run([GPRBUILD, '-P', tempgpr.name,
             '--autoconf=%s' % BUILDER.SUITE_CGPR] + extraopts)

        rm(tempgpr.name)


class RuntimeInfo(object):
    """Gather runtime-specific information and behaviors."""

    def __init__(self, runtime_name=None):
        self.runtime_name = runtime_name

        # Categorize the runtime we have. The selection of the instrumentation
        # strategy depends on it.
        self.has_full_runtime = False
        self.has_ravenscar_runtime = False
        self.has_kernel_runtime = False
        self.has_light_runtime = False
        self.has_exception_propagation = True

        if not self.runtime_name:
            self.has_full_runtime = True
        elif 'embedded' in self.runtime_name:
            self.has_ravenscar_runtime = True
        elif 'light-tasking' in self.runtime_name:
            self.has_ravenscar_runtime = True
            self.has_exception_propagation = False
        elif self.runtime_name.startswith('zfp'):
            self.has_light_runtime = True
            self.has_exception_propagation = False
        elif (
            self.runtime_name == "light" 
            or self.runtime_name.startswith('light-')
        ):
            self.has_light_runtime = True
            self.has_exception_propagation = False
        elif self.runtime_name == 'kernel':
            self.has_kernel_runtime = True
        else:
            assert False, 'Unknown runtime: {}'.format(runtime_name)

    @property
    def gnatcov_rts_project(self):
        """Name of the gnatcov_rts project to use in instrumented projects."""

        # gnatcov_rts_full and gnatcov_rts were merged with the introduction
        # of gnatcov setup.

        return (
            'gnatcov_rts_full' if (
                self.has_full_runtime and not gnatcov_info().has_setup)
            else 'gnatcov_rts'
        )

def runtime_info(runtime=None):
    if runtime is None:
        runtime = env.main_options.RTS
    return RuntimeInfo(runtime)


# Target specificities. We don't have many variants but need to match each
# against a few possible triplets.

class TargetInfo:
    """
    Gather target specific information and behaviors

    exeext (str): Filename extension for programs.

    partiallinks (bool): Whether the linker performs partial links.  Knowing
    this is needed to make -gc-sections work properly.

    to_platform_specific_symbol: Function that turns a platform-independent
    symbol into a platform specific one, or None if they are the same.  This
    enables us to use platform-independent symbol names in testcases.
    """
    def __init__(self, exeext, partiallinks, to_platform_specific_symbol=None):
        self.exeext = exeext
        self.partiallinks = partiallinks
        self.to_platform_specific_symbol = (
            to_platform_specific_symbol or (lambda x: x))


# For each family of targets we need to distinguish, a regexp to match against
# the actual target triplet and the corresponding TargetInfo data:

TARGETINFO = {
    # VxWorks targets
    '.*-vxworks': TargetInfo(exeext='.out', partiallinks=True),

    # x86-windows targets
    'i686.*-mingw': TargetInfo(
        exeext='.exe', partiallinks=False,
        to_platform_specific_symbol=lambda x: '_{}'.format(x)),

    # x86_64-windows targets
    'x86_64.*mingw': TargetInfo(exeext='.exe', partiallinks=False),

    # default
    '.': TargetInfo(exeext='', partiallinks=False)}


def target_info(target=None):
    """
    The TargetInfo data for the provided `target` triplet. If `target` is None,
    use the testsuite target.
    """

    if target is None:
        target = env.target.triplet

    for re_target in TARGETINFO:
        if re.match(pattern=re_target, string=target):
            return TARGETINFO[re_target]


class GnatcovInfo:
    def __init__(self):
        p = re.search(
                pattern="GNATcoverage (\d+)",
                string=version("gnatcov")
            )
        self._major = int(p.group(1)) if p else None
        self.has_setup = self.major_at_least(23)


    def major_at_most(self, val):
        """
        Returns whether the major version of gnatcov is less or equal than
        val. Consider that development versions of gnatcov have a greater
        major version than anything.
        """
        return self._major is not None and self._major <= val

    def major_at_least(self, val):
        """
        Returns whether the major version of gnatcov is greater or equal than
        val. Consider that development versions of gnatcov have a greater
        major version than anything.
        """
        return self._major is None or self._major >= val

    def major(self):
        """
        Major of gnatcov, if defined, or None if gnatcov is a development
        version.
        """
        return self._major

def gnatcov_info():
    return GnatcovInfo()

# Allowed pairs for the --gnatcov-<cmd> family of command line options:
ALTRUN_GNATCOV_PAIRS = (('gnatcov', 'run'), )

# Allowed pairs for the --pre/post-testsuite/testcase family of command line
# options:
ALTRUN_HOOK_PAIRS = (('pre', 'testsuite'),
                     ('post', 'testsuite'),
                     ('pre', 'testcase'),
                     ('post', 'testcase'))


def altrun_opt_for(p0, p1):
    """Name of the command line option controlling the ALTRUN (P0, P1) pair."""
    return "%s_%s" % (p0, p1)


def altrun_attr_for(p0, p1):
    """Name of our internal controlling options attribute for the
    ALTRUN (P0, P1) pair."""
    return "%s_%s" % (p0, p1)


def cargs_opt_for(lang):
    """Name of the command line option to pass for language LANG."""
    return "cargs" + (':%s' % lang if lang else "")


def cargs_attr_for(lang):
    """
    Name of our internal options attribute to hold cargs for language LANG.
    """
    return "cargs" + ('_%s' % lang if lang else "")


def add_shared_options_to(parser, toplevel):
    """
    Shared command line options.

    Options allowed at the testsuite.py level which need to be passed down to
    individual test.py.
    """
    # --gnatcov_<cmd> family
    for pgm, cmd in ALTRUN_GNATCOV_PAIRS:
        parser.add_argument(
            '--%s' % altrun_opt_for(pgm, cmd), dest=altrun_attr_for(pgm, cmd),
            metavar="CMD",
            help='Use CMD instead of "%s %s"' % (pgm, cmd))

    # Valgrind control
    parser.add_argument(
        '--enable-valgrind', dest='enable_valgrind',
        choices='memcheck callgrind'.split(),
        help='Enable the use of Valgrind (memcheck or callgrind) during the'
             ' test execution.')

    # RTS for tested programs. Defaulting to "" instead of None lets us
    # perform RE searches unconditionally to determine profile.
    parser.add_argument(
        '--RTS', dest='RTS', metavar='RTS', default="",
        help='--RTS option to pass to gprbuild, if any. Assume "full" profile'
             ' by default.')

    # --board
    parser.add_argument(
        '--board', dest='board', metavar='BOARD',
        help='Specific target board to exercize.')

    # --gprmode
    parser.add_argument(
        '--gprmode', dest='gprmode', action='store_true',
        help='Use -P instead of --scos for analysis on source coverage tests.')

    # --kernel
    parser.add_argument(
        '--kernel', dest='kernel', metavar='KERNEL',
        help='KERNEL to pass to gnatcov run in addition to exe.')

    # --trace-mode
    parser.add_argument(
        '--trace-mode', dest='trace_mode', metavar='TRACE_MODE',
        choices=('bin', 'src'), default='bin',
        help='Kind of execution traces to use for SCOV driven tests.'
             ' "bin" for binary traces out of valgrind or qemu,'
             ' "src" for source traces out of source level instrumentation.')

    # --trace-size-limit
    parser.add_argument(
        '--trace-size-limit', dest='trace_size_limit', metavar='TRSZ_LIMIT',
        help='Best effort request to the execution environment to stop when'
             ' the execution trace would grow beyond the provided size. Only'
             ' effective for qemu based executions, with values like "10M".')

    # --largs
    parser.add_argument(
        '--largs', dest='largs', metavar='LARGS', default='',
        help='-largs to pass to gprbuild.')

    # --cargs[:<lang>] family: a common, language agnostic, one + one for each
    # language we support. --cargs "" should be kept semantically equivalent
    # to absence of --cargs at all, and forcing a string allows simpler code
    # downstream.
    for lang in [None] + KNOWN_LANGUAGES:
        parser.add_argument(
            '--%s' % cargs_opt_for(lang), dest=cargs_attr_for(lang),
            metavar='CARGS', default='',
            help='Additional arguments to pass to the %scompiler when'
                 ' building test programs.' % ('%s ' % lang if lang else ''))

    # --auto-arch

    parser.add_argument(
        '--auto-arch', action='store_true',
        help='Autodetect which "gnatcov" to use (32-bit or 64-bit one).')

    # --consolidate
    parser.add_argument(
        '--consolidate', dest='consolidate', default="traces",
        help="Artifacts to be used for consolidation specs.",
        choices=('traces', 'checkpoints'))

    # --pretty-print
    parser.add_argument(
        '--pretty-print', action='store_true',
        help='Whether to pretty-print generated sources (for "gnatcov'
             ' instrument".')

    # --spark-tests
    parser.add_argument(
        '--spark-tests', dest="spark_tests", metavar='SPARK_TESTSUITE_PATH',
        help='Path to the SPARK testsuite. Activate SPARK-specific'
             ' tests. Implicitly appends -gnat12 to Ada cargs.')

    # --all-warnings
    parser.add_argument(
        '--all-warnings', action='store_true',
        help='Whether to pass --all-warnings to gnatcov.')

    parser.add_argument(
        '--rewrite', action='store_true',
        help='Rewrite test baselines according to current outputs.')

    # --default-dump-trigger
    parser.add_argument(
        '--default-dump-trigger', dest='default_dump_trigger',
        help='Default dump trigger to be passed to "gnatcov instrument,"'
             ' unless the test specifically overrides it.'
    )

    # --default-dump-channel
    parser.add_argument(
        '--default-dump-channel', dest='default_dump_channel',
        help='Default dump channel to be passed to "gnatcov instrument,"'
             ' unless the test specifically overrides it.'
    )
