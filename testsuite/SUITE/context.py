# ***************************************************************************
# **                             TEST SUITE-CONTEXT                        **
# ***************************************************************************

# This module is imported by all testcases. It exposes a single "thistest"
# instance of a Test class, to switch to the test subdir, and offer command
# line and test status management facilities.

# It also exposes a few global variables of general use (env, TEST_DIR, etc)

# ***************************************************************************

from gnatpython.ex import Run, PIPE
from gnatpython.env import Env
from gnatpython.main import Main

from gnatpython.fileutils import cd, rm, which, diff, touch, mkdir, ls, find

import os, re, sys

from SUITE import control
from SUITE.control import GPRCLEAN, BUILDER, LANGINFO, target_info

from SUITE.cutils import ndirs_in, lines_of
from SUITE.control import KNOWN_LANGUAGES

# This module is loaded as part of a Run operation for a test.py
# file found and launched by the toplevel driver

# This is where the toplevel invocation was issued:
ROOT_DIR = os.getcwd()

# And this is the relative directory where test.py was found:
TEST_DIR = os.path.dirname(sys.modules['__main__'].__file__)

env = Env()

# ==================
# == ReportOutput ==
# ==================

# Internal helper to dispatch information to test.py.err/log/out

class _ReportOutput(object):
    """A class that allows us to write some text to a report file, while
    bufferizing part of it until we know whether this part should also
    be printed on standard output or not.  The idea is to buffer the
    output generated for each driver until the end of the test, and then
    print that output to stdout if we then determine that the test failed.

    ATTRIBUTES
      report_fd: A file descriptor to the report file where all the output
            is always written.
      output: A string buffer holding the output being written to the report
            file.  The contents of that buffer may be reset after a driver
            has been run and associated results have been collected.  See
            method "flush" below.
      print_diff: A boolean, False by default, that should be true if
            the contents of the output attribute should be printed on
            standard output at the next flush.
    """
    def __init__(self, report_file):
        """Constructor.

        PARAMETERS
          report_file: The name of the file where to write all the logs.
        """
        self.report_file = report_file

        self.report_fd = open(self.report_file, "w")
        self.output = ""
        self.print_diff = False

    def enable_diffs(self):
        """Turn printing of the output buffer on.  The printing will be done
        at the next flush.
        """
        self.print_diff = True

    def log(self, text, end_of_line=True):
        """Write the given text in the output file.  This also adds
        the text to the output buffer.

        PARAMETERS
          text:   The text to be logged.
          end_of_line: If True, then append a '\n' character at the end
                  of text. This affects both the report file and the output
                  buffer. The idea is to emulate the "print" statement
                  which adds this '\n' by default too.
        """
        if end_of_line:
            text += '\n'
        self.output += text
        self.report_fd.write(text)

    def flush(self):
        """Reset the output buffer (printing its content on standard output
        first if print_diff is True).  Reset print_diff to False as well.
        """
        if self.print_diff:
            print self.output,
        self.output = ""
        self.print_diff = False
        self.report_fd.flush()

    def close(self):
        """Close the file descriptor for our report file.
        """
        self.report_fd.close()

# ==========
# == Test ==
# ==========

class Test (object):
    """Test class:

    Offer test command line and status management facilities.

    Command line options are made available as an "options" field.

    The success/failure of each individual test is managed a-la ACATS
    fashion. The user level test code is expected to

    - call "result" when the testing sequence is finished,

    - call "failed" or "stop" when it has detected something unexpected,
      and wishes processing to continue or stop, respectively.

      "fail_if"/"stop_if" interfaces are offered to make this straightforward
      in common situations.

    A test is either PASSED or FAILED. It is considered PASSED when
    no failure was registered.
    """

    # ---------------
    # -- __init __ --
    # ---------------

    def __init__(self):
        """Initialize the instance: switch to the test subdirectory, parse
        command line options, reset the failures counter and precompute
        gprbuild options we'll have to pass on every call to convey config
        options.
        """

        # Compute the depth of this test wrt testsuite root. We join ROOT and
        # TEST dirs then compute the length of the relative path, instead of
        # just counting the number of components in TEST_DIR, to prevent
        # inacuracies from possible "./" components that don't really increase
        # the depth.

        self.reldir = TEST_DIR
        self.homedir = os.path.join (ROOT_DIR, TEST_DIR)

        self.depth = ndirs_in (
            os.path.relpath (self.homedir, ROOT_DIR))

        cd(TEST_DIR)

        self.options = self.__cmdline_options()
        self.n_failed = 0
        self.report = _ReportOutput(self.options.report_file)
        self.current_test_index = 0

        self.gprconfoptions = [
            # verbose mode for verifiability in qualif mode.
            # quiet mode for performance (less io) otherwise.
            '-v' if self.options.qualif_level else '-q',

            # gprconfig base, selecting runtime
            '--config=%s' % os.path.join (ROOT_DIR, BUILDER.SUITE_CGPR)
            ]
        
        # In qualification mode, we expect the toplevel testsuite driver to
        # have dumped a level-tailored configuration pragma file for Ada,
        # e.g. to enforce Restrictions required to be honored at this level.

        if self.options.qualif_level:
            self.gprconfoptions.append (
                '-gnatec=%s/gnat.%s' % (ROOT_DIR, self.options.qualif_level)
                )

        self.gprvaroptions = [
            '-XTARGET=%s' % env.target.triplet]

        if self.options.board:
            self.gprvaroptions.append ('-XBOARD=%s' % self.options.board)

        self.tinfo = target_info ()

        # Whether this test will be using project files to locate SCOs when
        # running gnatcov.  This is decided on a per gnatcov invocation basis.
        # self.options.gnatcov states whether we're queried to do this for
        # SCOV driven tests.

        self.gprmode = False

        # Callgrind may be invoked more than once for each test. Memorize the
        # number of times it has been used in order to generate multiple logs.
        self.callgrind_count = 0

    # -------------
    # -- cleanup --
    # -------------

    def cleanup(self, project):
        """Cleanup possible remnants of previous builds."""

        Run([GPRCLEAN, "-P%s" % project]
            + self.gprconfoptions + self.gprvaroptions)
        rm('*.xcov')
        rm('*.bin')

    # ----------------------------
    # -- Test status management --
    # ----------------------------

    def log(self, text, new_line=True):
        """Calls self.report.log.
        """
        self.report.log(text, new_line)

    def flush(self):
        """Calls self.report.flush.
        """
        self.report.flush()

    def comment(self, text):
        """Output a TEXT comment."""
        self.log('  - %s.' % text)

    def failed(self, comment="assertion failed"):
        """Register a check failure."""
        self.log('  * %s' % comment)
        self.report.enable_diffs()
        self.n_failed += 1

    def fail_if(self, expr, comment="assertion failed"):
        """Register a check failure when EXPR is true."""
        if expr:
            self.failed (comment)

    def stop(self,exc):
        self.failed("Processing failed")
        self.result()
        raise exc

    def stop_if(self, expr, exc):
        if expr:
            self.stop(exc)

    def result(self):
        """Output the final result which the testsuite driver looks for.

        This should be called once at the end of the test
        """
        if self.n_failed == 0:
            self.log('==== PASSED ============================.')
        else:
            self.log('**** FAILED ****************************.')

        # Flush the output, in case we forgot to do so earlier.  This has no
        # effect if the flush was already performed.

        self.flush()
        self.report.close()

    # -------------------------
    # -- create_callgrind_id --
    # -------------------------

    def create_callgrind_id(self):
        """Return a test-unique ID to identify a callgrind run."""
        self.callgrind_count += 1
        return self.callgrind_count


    # -----------------------------
    # -- Test options management --
    # -----------------------------

    def __cmdline_options(self):
        """Return an options object to represent the command line options"""
        main = Main(require_docstring=False, add_targets_options=True)
        main.add_option('--timeout', dest='timeout', type=int,
                        default=None)
        main.add_option('--gprmode', dest='gprmode',
                        action='store_true', default=False)
        main.add_option('--enable-valgrind', dest='enable_valgrind',
                        action='store', choices='memcheck callgrind'.split(),
                        default=None)
        main.add_option('--trace_dir', dest='trace_dir', metavar='DIR',
                        help='Traces location. No bootstrap if not specified.',
                        default=None)
        main.add_option('--report-file', dest='report_file', metavar='FILE',
                        help='The filename where to store the test report '
                             '[required]')
        main.add_option('--qualif-level', dest='qualif_level',
                        metavar='QUALIF_LEVEL',
                        help='The target qualification level when we are '
                             'running in qualification mode.')

        # --cargs[:<lang>] family

        [main.add_option (
                '--cargs:%s' % lang, dest=self.__cargs_optvar_for (lang),
                metavar='CARGS_%s' % lang, default="",
                help=('Additional arguments to pass to the %s compiler '
                      'when building the test programs.' % lang)
                )
         for lang in KNOWN_LANGUAGES]

        main.add_option(
            '--cargs', dest=self.__cargs_optvar_for(lang=None),
            metavar='CARGS', default="",
            help=('Additional arguments to pass to the compiler '
                  'when building the test programs.')
            )

        # --gnatcov-<cmd> family

        [main.add_option(
                '--gnatcov-%s' % cmd, dest='gnatcov_%s' % cmd,
                default=None, help='program to use instead of "gnatcov %s"' % cmd,
                metavar="...")
         for cmd in control.GNATCOV_COMMANDS]

        main.add_option('--xcov-level', dest='xcov_level',
                        help='Force the --level argument passed to xcov '
                             'instead of deducing it from the test category '
                             'when that normally happens.')
        main.add_option('--board', dest='board', metavar='BOARD',
                        help='Specific target board to exercize')
        main.add_option('--RTS', dest='RTS', metavar='RTS', default="",
                     help='--RTS option to pass to gprbuild, if any.')
        main.add_option('--kernel', dest='kernel', metavar='KERNEL',
                     help='KERNEL to pass to gnatcov run in addition to exe')

        main.add_option('--toolchain', dest='toolchain', default="")

        main.add_option('--tags', dest='tags', default="")

        main.parse_args()
        if main.options.report_file is None:
            # This is a required "option" which is a bit self-contradictory,
            # but it's easy to do it that way.
            main.error("The report file must be specified with --report-file")

        # Get our tags set as a list. Fetch contents from file if needed
        # first:

        if main.options.tags and main.options.tags.startswith('@'):
            main.options.tags = ' '.join(
                lines_of (main.options.tags[1:])
                )
        if main.options.tags:
            main.options.tags = main.options.tags.split()

        return main.options

    def __cargs_optvar_for (self, lang):
        """Name of our options dictionary key name to hold cargs for LANG."""
        return "-cargs" + (':%s' % lang if lang else "")

    def suite_cargs_for (self, lang):
        """String of options passed as --cargs[:LANG] to the testsuite
        driver. None if no such option passed. LANG might be None, to fetch
        options passed as --cargs."""

        return thistest.options.__dict__ [self.__cargs_optvar_for (lang)]

    def support_dir(self):
        return os.path.join (ROOT_DIR, 'support')

# Instantiate a Test object for the individual test module that
# imports us.

thistest = Test ()

