# ***************************************************************************
# **                             TEST SUITE CONTROL                        **
# ***************************************************************************

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import rm
import os.path

env = Env()

# Append .exe on windows for native tools
GPRBUILD  = 'gprbuild' + env.host.os.exeext
GPRCONFIG = 'gprconfig' + env.host.os.exeext
GPRCLEAN  = 'gprclean' + env.host.os.exeext

XCOV      = 'gnatcov' + env.host.os.exeext

class LangInfo:
    """A class that provides some info about a given language.

    ATTRIBUTES:
        name:
          The name of the language. Eg: "Ada", or "C".

        src_ext:
           A list of extensions used for filenames of that language.
           Eg: [".ads", ".adb"] for GNAT, or [".h", ".c"] for C.

        scos_ext:
           The extension used for filenames that contain the SCOs used by
           xcov. For instance, in Ada, that would be ".ali".

        comment:
           The comment marker used by that language to specify the start of a
           comment that runs until the end of the current line.  For instance,
           in Ada, it would be '--'.

        cargs:
           String of compilation command line arguments applied for sources
           in this language. These should not influence code generation, as
           such options are expected to be controllable externally.

           This always includes the source coverage support switches, required
           for all source coverage tests, harmless for object coverage tests,
           and which should actually be used in this case as well if object
           coverage were to be qualified.
    """
    def __init__(self, name, src_ext, scos_ext, comment, cargs):
        # The parameters have the same meaning as the class'
        # attributes, with the following exceptions:
        #     src_ext: If only one filename extension is being used,
        #         it is acceptable to pass it directly, rather than
        #         passing a single-element list.  This constructor
        #         will automatically make the translation.
        if not isinstance(src_ext, list):
            src_ext = [src_ext]
        self.name = name
        self.src_ext = src_ext
        self.scos_ext = scos_ext
        self.comment = comment
        self.cargs = cargs

# A dictionary mapping a LangInfo instance to each known language.

LANGINFO = {
    "Ada":  LangInfo(name="Ada", src_ext=[".ads", ".adb"], scos_ext=".ali",
                     comment='--', cargs="-gnateS -gnat05"),
    "C":    LangInfo(name="C", src_ext=[".h", ".c"], scos_ext=".gli",
                     comment='//', cargs="-fdump-scos"),
    "Cons": LangInfo(name="Consolidation", src_ext=".txt", scos_ext=None,
                     comment='--', cargs=None)
    }

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

    # Common compilation args, passed to all build invocations

    COMMON_CARGS = " -g -fpreserve-control-flow "

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
                 '  for Languages use ("Asm", "C", "Ada");',
                 'end %(prjname)s;')
                ) % {'prjname': os.path.basename(tempgpr.name).split('.')[0]}
            )
        tempgpr.close()

        # We now run gprbuild -Ptemp.gpr --target=bla --RTS=blo, which
        # will complain about missing sources, but only after producing
        # an automatic config file with everything we need, and nothing
        # else (no other file).

        # Use the requested --RTS for Ada, default to zfp if unspecified

        rm (BUILDER.SUITE_CGPR)

        Run ([GPRBUILD, '-P', tempgpr.name,
              '--RTS=%s' % toplev_options.RTS,
              '--target=%s' % env.target.triplet,
              '--autoconf=%s' % BUILDER.SUITE_CGPR]
             )

        rm (tempgpr.name)
