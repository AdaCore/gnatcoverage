# ***************************************************************************
# **                             TEST SUITE CONTROL                        **
# ***************************************************************************

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import rm
import os.path, re

from SUITE.cutils import no_ext

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
        ext=env.host.os.exeext
    )

# Append .exe on windows for native tools
GPRBUILD  = 'gprbuild' + env.host.os.exeext
GPRCONFIG = 'gprconfig' + env.host.os.exeext
GPRCLEAN  = 'gprclean' + env.host.os.exeext

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
    """
    def __init__(self, name, src_ext, comment, scofile_for):
        self.name = name
        self.src_ext = src_ext
        self.comment = comment
        self.scofile_for = scofile_for

# A dictionary mapping a LangInfo instance to each known language.

LANGINFO = {
    "Ada": LangInfo(
        name="Ada", src_ext=[".ads", ".adb"], comment='--',
        scofile_for=lambda source: (no_ext(source)+'.ali')),

    "C": LangInfo(
        name="C", src_ext=[".h", ".c"],  comment='//',
        scofile_for=lambda source: (source+'.gli')),

    "C++": LangInfo(
        name="C++", src_ext=[".hpp", ".cpp", ".cc", ".hh", ".hxx"],
        comment='//',
        scofile_for=lambda source: '{}.gli'.format(source)),

    "Asm": LangInfo(
        name="Asm", src_ext=[".s"], comment='#',
        scofile_for=None),

    "Cons": LangInfo(
        name="Consolidation", src_ext=[".txt"], comment='--',
        scofile_for=None)
    }

KNOWN_LANGUAGES = [li.name for li in LANGINFO.values() if li.scofile_for]
# list of languages that gnatcov supports

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

    # Common compilation args, passed to all build invocations.

    @staticmethod
    def __TARGET_CARGS_FOR(triplet):
        # Coverage analysis relies on advanced dwarf info disabled
        # by default on VxWorks.
        if 'vxworks' in triplet:
            return ['-gno-strict-dwarf']
   	return []
 
    @staticmethod
    def COMMON_CARGS():
        return (
            ["-g", "-fpreserve-control-flow", "-fdump-scos"]
            + BUILDER.__TARGET_CARGS_FOR (Env().target.triplet)
            )

    # Base command for a build

    BASE_COMMAND = GPRBUILD

    # Configuration file suitable for all the builder invocations,
    # setup early, once, to latch the compiler paths and RTS settings

    SUITE_CGPR = "suite.cgpr"

    @staticmethod
    def RUN_CONFIG_SEQUENCE (toplev_options):
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

        tempgpr = open ("suite.gpr", "w")

        tempgpr.write ('\n'.join (
                ('project %(prjname)s is',
                 '  for Languages use ("Asm", "C", "Ada", "C++");',
                 'end %(prjname)s;')
                ) % {'prjname': os.path.basename(tempgpr.name).split('.')[0]}
            )
        tempgpr.close()

        # We now run gprbuild -Ptemp.gpr --target=bla --RTS=blo, which
        # will complain about missing sources, but only after producing
        # an automatic config file with everything we need, and nothing
        # else (no other file).

        rm (BUILDER.SUITE_CGPR)

        extraopts = []
        if toplev_options.RTS:
            extraopts.append ('--RTS=%s' % toplev_options.RTS)

        Run ([GPRBUILD, '-P', tempgpr.name,
              '--target=%s' % env.target.triplet,
              '--autoconf=%s' % BUILDER.SUITE_CGPR
              ] + extraopts)

        rm (tempgpr.name)

# ===============================
# == libsupport considerations ==
# ===============================

# We rely on our support lib to provide a common last chance handler in every
# configuration where this makes sense, in particular with ZFP and Ravenscar
# RTS libraries.
#
# * ZFP profiles because some of these don't provide a handler at all.
#
# * Ravenscar because some handlers fallthrough to a infinite idle loop,
#   unfriendly wrt testcase termination in automated nightly executions.
#
# In addition, providing our last chance handler ensures we get consistent
# output on unexpected exception, on any target configuration.
#
# We can't override "last chance" handling and don't really need to for full
# runtimes (which terminate on exceptions), native or cross.

# This function controls whether we build the library and link with it

def need_libsupport ():
    return re.search ("zfp|ravenscar", env.main_options.RTS)

# ==========================
# == target specificities ==
# ==========================

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
    def __init__ (self, exeext, partiallinks, to_platform_specific_symbol=None):
        self.exeext = exeext
        self.partiallinks = partiallinks
        self.to_platform_specific_symbol = to_platform_specific_symbol or (lambda x: x)

TARGETINFO = {
    "powerpc-wrs-vxworks": TargetInfo (
        exeext = ".out", partiallinks=True
        ),
    "e500v2-wrs-vxworks": TargetInfo (
        exeext = ".out", partiallinks=True
        ),
    "i686-pc-mingw32": TargetInfo (
        exeext = ".exe", partiallinks=False,
        to_platform_specific_symbol=lambda x: '_{}'.format(x)
        ),
    "x86_64-pc-mingw32": TargetInfo (
        exeext = ".exe", partiallinks=False
        ),
    "default": TargetInfo (
        exeext = "", partiallinks=False
        )
}

def target_info (target=None):
    if target is None:
        target = env.target.triplet
    return (
        TARGETINFO [target] if target in TARGETINFO
        else TARGETINFO["default"])

# Allowed pairs for the --gnatcov-<cmd> family of command line options:

ALTRUN_GNATCOV_PAIRS = (
    ('gnatcov', 'run'),
    )

# Allowed pairs for the --pre/post-testsuite/testcase family of command line
# options:

ALTRUN_HOOK_PAIRS = (
    ('pre', 'testsuite'),
    ('post', 'testsuite'),
    ('pre', 'testcase'),
    ('post', 'testcase')
    )

def altrun_opt_for(p0,p1):
    """Name of the command line option controlling the ALTRUN (P0, P1) pair."""
    return "%s_%s" % (p0, p1)

def altrun_attr_for(p0,p1):
    """Name of our internal controlling options attribute for the
    ALTRUN (P0, P1) pair."""
    return "%s_%s" % (p0, p1)

def cargs_opt_for (lang):
    """Name of the command line option to pass for language LANG."""
    return "cargs" + (':%s' % lang if lang else "")

def cargs_attr_for (lang):
    """Name of our internal options attribute to hold cargs for
    language LANG."""
    return "cargs" + ('_%s' % lang if lang else "")

# =================================
# == Shared command line options ==
# =================================

# Options are allowed at the testsuite.py level which need to be
# passed down to individual test.py.

def add_shared_options_to (o, toplevel):

    # --gnatcov_<cmd> family

    [o.add_option(
            '--%s' % altrun_opt_for(pgm, cmd), dest=altrun_attr_for(pgm, cmd),
            default=None, help='use CMD instead of "%s %s"' % (pgm, cmd),
            metavar="CMD")
     for (pgm, cmd) in ALTRUN_GNATCOV_PAIRS]

    # valgrind control

    o.add_option(
        '--enable-valgrind', dest='enable_valgrind',
        choices='memcheck callgrind'.split(),
        default=None,
        help=('enable the use of Valgrind (memcheck or callgrind)'
              'during the test execution.')
        )

    # RTS for tested programs. Defaulting to "" instead of None lets us
    # perform RE searches unconditionally to determine profile.

    o.add_option(
        '--RTS', dest='RTS', metavar='RTS', default="",
        help=('--RTS option to pass to gprbuild, if any. '
              'Assume "full" profile by default.')
        )

    # --board

    o.add_option(
        '--board', dest='board', metavar='BOARD',
        help='Specific target board to exercize'
        )

    # --gprmode

    o.add_option(
        '--gprmode', dest='gprmode', action='store_true',
        default=False,
        help='Use -P instead of --scos for analysis on source coverage tests'
        )

    # --kernel

    o.add_option(
        '--kernel', dest='kernel', metavar='KERNEL',
        help='KERNEL to pass to gnatcov run in addition to exe'
        )
    
    # --toolchain

    o.add_option(
        '--toolchain', dest='toolchain', metavar='TOOLCHAIN', default="",
        help=('Prefix of the toolchain to use to compile tests' if toplevel
              else 'Toolchain discriminant')
        )

    # --largs

    o.add_option(
        '--largs', dest='largs', metavar='LARGS', default="",
        help=('-largs to pass to gprbuild')
        )

    # --cargs[:<lang>] family: a common, language agnostic, one + one for each
    # language we support. --cargs "" should be kept semantically equivalent
    # to absence of --cargs at all, and forcing a string allows simpler code
    # downstream.

    [o.add_option (
            "--%s" % cargs_opt_for(lang), dest=cargs_attr_for(lang),
            metavar='CARGS', default="",
            help=(
                'Additional arguments to pass to the %scompiler when '
                'building test programs.' % ("%s " % lang if lang else ""))
            )
     for lang in [None] + KNOWN_LANGUAGES]

    # --auto-arch

    o.add_option(
        '--auto-arch', action='store_true',
        help='Autodetect which "gnatcov" to use (32-bit or 64-bit one)'
    )

    # --consolidate

    o.add_option(
        '--consolidate', dest='consolidate', default="traces",
        help=("artefacts to be used for consolidation specs"),
        choices=('traces', 'checkpoints')
        )
