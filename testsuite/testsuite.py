#!/usr/bin/env python

# ***************************************************************************
# ***                  COUVERTURE TESTSUITE MAIN DRIVER                   ***
# ***************************************************************************

"""./testsuite.py [OPTIONS] [TEST_PATH]

Run the couverture testsuite

To run tests whose relative path to test.py match a provided regexp
   ./testsuite.py I401-009
   ./testsuite.py ./tests/I4

See ./testsuite.py -h for more help
"""

# ***************************************************************************

from gnatpython.env import Env, getenv, putenv
from gnatpython.ex import Run
from gnatpython.fileutils import mkdir, rm, ln, find, which
from gnatpython.main import Main
from gnatpython.mainloop import MainLoop

from gnatpython.optfileparser import OptFileParse
from gnatpython.reports import ReportDiff

from glob import glob

import time
import logging, os, re, sys

from SUITE import cutils
from SUITE.cutils import contents_of, re_filter, clear, to_list, FatalError
from SUITE.cutils import version

from SUITE.qdata import QDregistry, QDreport, qdaf_in, QLANGUAGES, QROOTDIR

from SUITE import control
from SUITE.control import BUILDER, XCOV

DEFAULT_TIMEOUT = 600

# ==========================================
# == Qualification principles and control ==
# ==========================================

# The testsuite tree features particular subdirectories hosting TOR and
# qualification testcases. These are all hosted down a single root directory,
# and we designate the whole piece as the qualification subtree.

# These tests may be run as part of a regular testing activity or for an
# actual qualification process. The latter is indicated by passing
# --qualif-level on the command line, in which case the testsuite is said to
# run in qualification mode.

# The qualification mode aims at producing a test-results qualification report
# for the provided target level.

# Beyond the production of a qualification report, --qualif-level has several
# effects of note:
#
#   * The set of tests exercised is restricted to the set of qualification
#     tests relevant for the target level,
#
#   * The coverage analysis tool is called with a --level corresponding to the
#     target qualification level for all the tests, whatever the criterion the
#     test was designed to assess. For example, for a target level A we will
#     invoke gnatcov --level=stmt+mcdc even for tests designed to verify
#     statement coverage only.
#
#   * For criteria with variants (e.g. unique-cause and masking mcdc),
#     exercise only the default one.

# A dictionary of information of interest for each qualification level:

class QlevelInfo:
    def __init__(self, levelid, subtrees, xcovlevel):
        self.levelid   = levelid   # string identifier

        # regexp of directory subtrees: testdirs that match this
        # hold qualification tests for this level
        self.subtrees  = subtrees

        # --level argument to pass to xcov when running such tests when in
        # qualification mode
        self.xcovlevel = xcovlevel

RE_QCOMMON="(Common|Appendix)"
RE_QLANG="(%s)" % '|'.join (QLANGUAGES)

# A regular expression that matches subdirs of qualification tests that
# should apply for coverage criteria RE_CRIT.

def RE_SUBTREE (re_crit):
    return "%(root)s/((%(common)s)|(%(lang)s/(%(crit)s)))" % {
        "root": QROOTDIR, "common": RE_QCOMMON,
        "lang": RE_QLANG, "crit": re_crit
        }

QLEVEL_INFO = {

    "doA" : QlevelInfo (
        levelid   = "doA",
        subtrees  = RE_SUBTREE (re_crit="stmt|decision|mcdc"),
        xcovlevel = "stmt+mcdc"),

    "doB" : QlevelInfo (
        levelid   = "doB",
        subtrees  = RE_SUBTREE (re_crit="stmt|decision"),
        xcovlevel = "stmt+decision"),

    "doC" : QlevelInfo (
        levelid   = "doC",
        subtrees  = RE_SUBTREE (re_crit="stmt"),
        xcovlevel = "stmt")
    }


# ===============================
# == Compilation Flags Control ==
# ===============================

# The --cargs family of options controls the compilation options used to
# compile all the tests, part of the qualification tree or not. This allows
# exercising all the tests with nightly variants, not only qualification
# tests.

# --cargs options are conveyed as CARGS_ discriminants, with leading dashes
# stripped and without language indication. For example --cargs="-O1"
# --cargs:Ada="-gnatp" translates as CARGS_O1 + CARGS_gnatp discriminants.

# Individual tests that really depend on particular options might of course
# request so, via:
#
# - The "extracargs" argument to the gprbuild function in tutils, for
#   tests that invoke this function directly from test.py, or
#
# - The "extracargs" initializer argument of TestCase instances
#
# This is not allowed for qualifcation tests though, because we need simple
# and reliable ways to determine the set of options we support and too many
# possible sources for options is unwelcome.

# On top of this user level control, we add a couple of flags such as
# -fdump-scos automatically. Below is a sketch of the internal compilation
# flags flow:
#
#     gprfor ()
#        template.gpr
#        % Switches (main) += "-fno-inline" as needed
#             |
#             |   direct calls to gprbuild() from test.py,
#             |   or via TestCase(extracargs)
#             |       |
#             v       v           testsuite.py
#  gprbuild (gpr, extracargs)     [--cargs=<>] [--cargs:Ada=<>] [--cargs:C=<>]
#                     |               |
#                     o----> ADD <----o
#                             |
#  SUITE.control.             v
#  BUILD.COMMON_CARGS   ---> ADD
#  (-g -fdump-scos ...)       |
#                             v
#     run "gprbuild -Pgpr --cargs=... [-cargs:Ada=<>] [-cargs:C=<>]

# In addition to the SUITE.control bits, the only default option we enforce is
# -gnat05 for Ada.

# ===============
# == TestSuite ==
# ===============

class TestSuite:

    # --------------------------
    # -- GAIA file facilities --
    # --------------------------

    def __init_logdir (self):

        self.log_dir = os.path.join (os.getcwd(), 'output')
        mkdir(self.log_dir)

        [open(os.path.join(self.log_dir, f), 'w').close()
         for f in ('comment', 'results', 'discs')]

    def __push_log (self, textlist, filename):
        """Append the list of lines in TEXTLIST to the GAIA log FILENAME."""

        with open(os.path.join(self.log_dir, filename), mode='a') as fd:
            fd.write ('\n'.join (textlist) + '\n')

    def __push_comments (self, textlist):
        self.__push_log (
            textlist = textlist, filename = 'comment')

    def __push_results (self, textlist):
        self.__push_log (
            textlist = textlist, filename = 'results')

    # ------------------------
    # -- Object constructor --
    # ------------------------

    def __init__(self):
        """Prepare the testsuite run: parse options, compute and dump
        discriminants, compute lists of dead/non-dead tests, run gprconfig and
        build the support library for the whole series of tests to come"""

        # Parse command lines options, also setting self.enable_valgrind
        # to convey whether tests should be run under valgrind control:

        self.options = self.__parse_options()

        # Add current directory in PYTHONPATH, allowing TestCases to find the
        # SUITE and SCOV packages:

        self.env = Env()
        self.env.add_search_path('PYTHONPATH', os.getcwd())

        # Perform the environment adjustments required to run the compilation
        # toolchain properly:

        self.setup_toolchain (self.options.toolchain)

        # Setup the log directory and initialize the GAIA log files:

        self.__init_logdir ()

        # Setup trace directories for bootstrap runs:

        if self.options.bootstrap_scos != None:
            self.trace_dir = os.path.join (self.log_dir, 'traces')
            rm(self.trace_dir, recursive=True)
            mkdir(self.trace_dir)
        else:
            self.trace_dir = None

        # Generate the discs list for test.opt parsing, and dump the list in a
        # file we can then use that file to determine which discriminants were
        # set during a particular run:

        self.discriminants = self.__discriminants()
        self.__push_log (
            textlist = [" ".join(self.discriminants)],
            filename = "discs"
            )

        # Dump useful comments about this run for starters

        self.__push_comments (self.__early_comments())

        # Run the builder configuration for the testsuite as a whole. Doing
        # it here once both factorizes the work for all testcases and prevents
        # cache effects if PATH changes between testsuite runs.

        BUILDER.RUN_CONFIG_SEQUENCE (self.options)

        # Build support library as needed

        if control.need_libsupport():

            targetargs = ["TARGET=%s" % self.env.target.triplet]
            if self.options.board:
                targetargs.append ("BOARD=%s" % self.options.board)

            logfile = os.path.join (self.log_dir, 'build_support.out')

            p = Run(['make', '-C', 'support', '-f', 'Makefile.libsupport']
                    + targetargs + ["RTS=%s" % self.options.RTS],
                    output=logfile)

            if p.status != 0:
                raise FatalError (
                    ("Problem during libsupport construction. %s:\n" % logfile)
                    + contents_of (logfile))

        # Instanciate what we'll need to produce a qualfication report.
        # Do that always, even if not running for qualif. The registry will
        # just happen to be empty if we're not running for qualif.

        self.qdreg = QDregistry()

        # Initialize counter of consecutive failures, to stop the run
        # when it is visibly useless to keep going

        self.n_consecutive_failures = 0

    # -----------------------------------
    # -- Early comments about this run --
    # -----------------------------------

    def __options_comment (self):
        return "Testsuite options:\n" + " ".join(_quoted_argv())

    def __versions_comment (self):

        prefix = (
            self.env.target.triplet + '-'
            ) if self.env.main_options.target else ""

        all_versions = [
            version ("gnatcov") + ", " + version (prefix+"gnatls")
            ]

        if self.env.main_options.target:
            all_versions.append (version (prefix+"gnatemu", nlines=2))

        return '\n'.join (
            ["Running versions:"] + all_versions) + '\n'

    def __early_comments (self):
        return '\n\n'.join (
            [self.__options_comment (),
             self.__versions_comment ()])

    # -------------------------------
    # -- Discriminant computations --
    # -------------------------------

    def __discriminants (self):
        """Full set of discriminants that apply to this test"""
        return (
            self.__base_discriminants()
            + self.__qualif_level_discriminants()
            + self.__cargs_discriminants()
            + self.__rts_discriminants()
            + self.__toolchain_discriminants())

    def __base_discriminants(self):
        return ['ALL'] + self.env.discriminants

    def __cargs_discriminants(self):
        """Compute a list of discriminants (string) for each switch passed in
        all the --cargs command-line option(s).  The format of each
        discriminant CARGS_<X> where <X> is the switch stripped of its
        leading dashes.

        For instance, if this testsuite is called with --cargs='-O1'
        --cargs-Ada='-gnatp', then this function should return
        ['CARGS_gnatp', 'CARGS_O1'].

        Return an empty list if --cargs was not used.
        """

        allopts = ' '.join (
            [self.env.main_options.__dict__[opt] for opt in
             ("cargs" + ext
              for ext in [""] + ["_%s" % l for l in QLANGUAGES])]
            )
        return ["CARGS_%s" % arg.lstrip('-') for arg in allopts.split()]

    def __qualif_level_discriminants(self):
        """List of single discriminant (string) denoting our current
        qualification mode, if any. This is ['QUALIF_LEVEL_XXX'] when invoked
        with --qualif-level=XXX, [] otherwise"""

        return (
            [] if not self.env.main_options.qualif_level
            else ["QUALIF_LEVEL_%s" % self.env.main_options.qualif_level]
            )

    def __rts_discriminants(self):
        """Compute a list of discriminant strings that reflect the kind of
        runtime support library in use, as conveyed by the --RTS command-line
        option."""

        # --RTS=zfp is strict zfp, missing malloc, memcmp, memcpy and put

        if self.env.main_options.RTS == "zfp":
            return ["RTS_ZFP_STRICT"]

        # ex --RTS=powerpc-elf/zfp-prep

        elif re.search ("zfp", self.env.main_options.RTS):
            return ["RTS_ZFP"]

        # ex --RTS=powerpc-elf/ravenscar-sfp-prep or --RTS=ravenscar-sfp

        elif re.search ("ravenscar.*sfp", self.env.main_options.RTS):
            return ["RTS_RAVENSCAR", "RTS_RAVENSCAR_SFP"]

        # ex --RTS=powerpc-elf/ravenscar-full-prep or --RTS=ravenscar

        elif re.search ("ravenscar", self.env.main_options.RTS):
            return ["RTS_RAVENSCAR", "RTS_RAVENSCAR_FULL"]

        # ex --RTS=native or --RTS=kernel

        else:
            return ["RTS_FULL"]

    def __toolchain_discriminants (self):
        """Compute the list of discriminants that reflect the version of the
        particular toolchain in use, if any, for example "7.0.2" for
        /path/to/gnatpro-7.0.2. The match is on the sequence of three single
        digits separated by dots, possibly followed by "rc", then by maybe
        a '/' prior to the end of string."""

        m = re.search ("(\d\.[01]\.[0123](?:rc)?)/?$", self.options.toolchain)
        return [m.group(1)] if m else []

    # ---------------------
    # -- __next_testcase --
    # ---------------------

    def __next_testcase_from (self, root):
        """Helper generator function for __next_testcase, producing a sequence
        of testcases to be executed from a provided root directory, updating
        self.run_list and self.dead_list on the fly."""

        if not self.options.quiet:
            logging.info(
                "Searching for tests, %s ...",
                ("matching '%s' from %s" % (
                        self.tc_filter if self.tc_filter else "unfiltered",
                        root))
                )

        test_py = "test.py"

        for (dirname, subdirs, files) in os.walk(
            top=root, topdown=True, followlinks=True
            ):

            if (test_py in files and
                re.search (
                    pattern=self.tc_filter, string=dirname)
                ):

                tc = TestCase (
                    os.path.join (dirname, test_py), self.trace_dir
                    )
                tc.parseopt(self.discriminants)

                if tc.killcmd:
                    self.dead_list.append(tc)
                else:
                    self.run_list.append(tc)
                    yield tc

    def __next_testcase (self):
        """Generator for MainLoop, producing a sequence of testcases to be
        executed, updating self.run_list and self.dead_list on the fly."""

        return (
            tc for root in ("./Qualif", "./tests")
            for tc in self.__next_testcase_from (root)
            )

    # ---------
    # -- run --
    # ---------

    def run (self):

        # Main loop : run all the tests and collect the test results, then
        # generate the human readable report. Make sure we produce a report
        # and keep going on exception as well, e.g. on stop for consecutive
        # failures threshold.

        self.run_list = []
        self.dead_list = []

        self.tc_filter = (
            self.options.run_test if self.options.run_test
            else "." if not self.options.qualif_level
            else "(%s)|Z999" % (
                QLEVEL_INFO[self.options.qualif_level].subtrees)
            )

        try :
            MainLoop(
                self.__next_testcase (),
                self.run_testcase, self.collect_result,
                self.options.jobs
                )

        except Exception as e:
            logging.info("Mainloop stopped on exception occurrence")
            logging.info(e.__str__())

            self.__push_comments (
                ["!!! MAINLOOP STOPPED ON EXCEPTION !!!", e.__str__()]
                )

        ReportDiff(
            self.log_dir, self.options.old_res
            ).txt_image('rep_gnatcov')

        # Report all dead tests
        self.__push_results (
            ["%s:%s" % (tc.rname(), tc.killcmd)
             for tc in self.dead_list]
            )

        # Warn about an empty non-dead list, always. This is almost
        # certainly a selection mistake in any case.

        if not self.run_list:
            logging.warning (
                "List of non-dead tests to run is empty. Selection mistake ?")

        # Otherwise, advertise the number of tests we have run, even in quiet
        # mode so we have minimum feedback to match against the intent.

        else:
            logging.info (
                "%d tests executed, %d dead or skipped" % (
                    len(self.run_list), len (self.dead_list))
                )

        # Generate bootstrap results
        if self.options.bootstrap_scos != None:

            # Generate trace list file
            trace_list = glob(self.trace_dir + '/*/*.trace')
            with open(self.trace_dir + '/trace.list', 'w') as file:
                file.write("\n".join(trace_list))

            Run(['time', which(XCOV), 'coverage', '--level=stmt',
                 '--scos=@' + self.options.bootstrap_scos, '--annotate=html',
                 '@' + self.trace_dir + '/trace.list',
                 '--output-dir=' + self.trace_dir],
                output=os.path.join(self.log_dir, 'bootstrap.out'))

    # ------------------
    # -- run_testcase --
    # ------------------

    def run_testcase(self, test, _job_info):
        """MainLoop hook to run a single non-dead TEST instance. If limit is
        not set, run rlimit with DEFAULT_TIMEOUT"""

        logging.debug("Running " + test.testdir)
        timeout = test.getopt('limit')
        if timeout is None:
            timeout = DEFAULT_TIMEOUT

        # Setup test execution related files. Clear them upfront to prevent
        # accumulation across executions and bogus reuse of old contents if
        # running the test raises a premature exception, before the execution
        # script gets a chance to initialize the file itself.

        outf = test.outf()
        logf = test.logf()
        diff = test.diff()
        qdaf = test.qdaf()

        [cutils.clear (f) for f in (outf, logf, diff, qdaf)]

        testcase_cmd = [sys.executable,
                        test.filename,
                        '--report-file=' + outf,
                        '--log-file=' + logf,
                        '--target', self.env.target.platform,
                        '--timeout', str(timeout)]
        if self.enable_valgrind:
            testcase_cmd.append('--enable-valgrind')
        if self.trace_dir is not None:
            test_trace_dir = os.path.join(test.trace_dir, str(test.index))
            mkdir(test_trace_dir)
            testcase_cmd.append('--trace_dir=%s' % test_trace_dir)

        # Propagate our command line arguments as testcase options.
        #
        # Beware that we're not using 'is not None' on purpose, to prevent
        # propagating empty arguments.

        mopt = self.env.main_options

        qlevels = test.qualif_levels ()

        # In qualification mode, pass the target qualification level to
        # qualification tests and enforce the proper xcov-level

        if mopt.qualif_level and qlevels:
            testcase_cmd.append('--qualif-level=%s' % mopt.qualif_level)
            testcase_cmd.append(
                '--xcov-level=%s' % QLEVEL_INFO[mopt.qualif_level].xcovlevel)

        # Pass cargs for all the tests, qualif family or not, qualif mode
        # or not.  Tests are not necessarily mono-language so we pass per
        # language cargs as well. Enforce -gnat05 by default for Ada.

        if not re.search (
            "-gnat95|-gnat05|-gnat12", mopt.cargs_Ada
            ):
            mopt.cargs_Ada += " -gnat05"

        [testcase_cmd.append(
                '--cargs:%(lang)s=%(args)s' % {
                    "lang" : lang,
                    "args" : mopt.__dict__ ["cargs_" + lang]
                    }
                )
         for lang in QLANGUAGES]

        testcase_cmd.append('--cargs=%s' % mopt.cargs)

        if mopt.board:
            testcase_cmd.append('--board=%s' % mopt.board)

        if mopt.gprmode:
            testcase_cmd.append('--gprmode')

        # If we have a kernel argument, resolve to fullpath now, providing
        # straightforward visibility to local test.py instances downtree.

        if mopt.kernel:
            testcase_cmd.append('--kernel=%s' % os.path.abspath (mopt.kernel))

        testcase_cmd.append('--RTS=%s' % mopt.RTS)

        test.start_time = time.time()

        return Run(testcase_cmd, output=diff, bg=True,
                   timeout=int(timeout) + DEFAULT_TIMEOUT)

    # --------------------
    # -- collect_result --
    # --------------------

    def collect_result(self, test, _process, _job_info):
        """MainLoop hook to collect results for a non-dead TEST instance."""

        # Several things to do once a test has run:
        # - logging (to stdout) the general test status,
        # - append a status summary to the "output/results" file,
        #   for our nightly infrastructure,
        # - see if there's a testcase object pickled around,
        #   to fetch back for the generation of a qualification
        #   test-results aggregate report.

        test.end_time = time.time()

        # Compute a few useful facts: Whether the test passed or failed,
        # if it was xfailed, with what comment, what was the error log when
        # the test failed, ...

        # Compute the actual execution status, what really happened whatever
        # what was expected;

        outf = test.outf()
        success = (
            cutils.match("==== PASSED ==================", outf)
            if os.path.exists(outf) else False)

        # If the execution failed, arrange to get a link to the err log
        # where the infrastructure expects it (typically not in the test
        # dedicated subdirectory where the original log resides)

        if not success:
            odiff = self.odiff_for(test)
            cutils.clear (odiff)
            ln(test.diff(), odiff)

        # Compute the status of this test (OK, UOK, FAILED, XFAIL) from
        # the combination of its execution success and a possible failure
        # expectation

        xfail_comment = test.getopt('xfail', None)
        xfail = xfail_comment is not None

        failed_comment = test.getopt('failed', None)

        comment = xfail_comment if xfail_comment else failed_comment

        status_dict = {
            # XFAIL?   PASSED? => status   PASSED? => status
              True:    {True:    'UOK',    False:    'OK'},
              False:   {True:    'XFAIL',  False:    'FAILED'}}

        status = status_dict[success][xfail]

        # Now log and populate "results" file

        # Avoid \ in filename for the final report
        test.filename = test.filename.replace('\\', '/')

        # File the test status + possible comment on failure

        self.__push_results ([
                ''.join (
                    ["%s:%s" % (test.rname(), status),
                     ":%s" % comment.strip('"') if not success and comment
                     else ""]
                    )
             ])

        # Log status as needed. All tests are logged in !quiet mode.
        # Real failures are always logged.

        dsec = test.end_time - test.start_time

        if (not self.options.quiet) or (not success and not xfail):
            logging.info (
                "%-68s %s - %s %s" % (
                    test.filename,
                    "%02d m %02d s" % (dsec / 60, dsec % 60),
                    status, "(%s)" % comment if comment else "")
                )

        # Dump errlog on unexpected failure

        if self.options.diffs and not success and not xfail:
            logging.info("Error log:\n" + contents_of (test.diff()))

        # Check if we have a qualification data instance pickled around,
        # and register it for later test-results production

        self.qdreg.check_qdata (
            qdaf=test.qdaf(), status=status, comment=comment)

        # Check if we need to stop the Suite as a whole

        if status == 'FAILED':
            self.n_consecutive_failures += 1
        else:
            self.n_consecutive_failures = 0

        if self.n_consecutive_failures >= 10:
            raise FatalError (
                "Stopped after %d consecutive failures"
                % self.n_consecutive_failures)

    def odiff_for(self, test):
        """Returns path to diff file in the suite output directory.  This file
        is used to generate report and results files."""

        filename = test.filename.replace('test.py', '')
        if filename.startswith('./'):
            filename = filename[2:]
        filename = filename.strip('/').replace('/', '-')
        return os.path.join(self.log_dir, filename + '.out')

    # -------------------
    # -- parse_options --
    # -------------------

    def __parse_options(self):
        """Parse command lines options"""

        m = Main(add_targets_options=True)
        m.add_option('--quiet', dest='quiet', action='store_true',
                     default=False, help='Quiet mode. Display test failures only')
        m.add_option('--gprmode', dest='gprmode', action='store_true',
                     default=False, help='Use -P instead of --scos')
        m.add_option('--diffs', dest='diffs', action='store_true',
                     default=False, help='show diffs on stdout')
        m.add_option('--enable-valgrind', dest='enable_valgrind',
                     action='store_true', default=False,
                     help='enable the use of valgrind when running each test')
        m.add_option('-j', '--jobs', dest='jobs', type='int',
                     metavar='N', default=1, help='Allow N jobs at once')
        m.add_option("--old-res", dest="old_res", type="string",
                        help="Old testsuite.res file")

        # cargs family: a common, language agnostic, one + one for each
        # language we support. Iterations on cargs wrt languages will be
        # performed using explicit references to the attribute dictionary of
        # m.options. Provide a default to allow straight access from such
        # iterations, without having to test.

        m.add_option('--cargs', dest='cargs', metavar='ARGS',
                     default="",
                     help='Additional arguments to pass to the compiler '
                          'when building the test programs. Language agnostic.')

        [m.add_option(
                '--cargs:%s' % lang, dest='cargs_%s' % lang,
                default="", help='cargs specific to %s tests' % lang,
                metavar="...")
         for lang in QLANGUAGES]

        m.add_option('--qualif-level', dest='qualif_level',
                     type="choice", choices=QLEVEL_INFO.keys(),
                     metavar='QUALIF_LEVEL',
                     help='State we are running in qualification mode for '
                          'a QUALIF_LEVEL target. This selects a set of '
                          'applicable tests for that level.')
        m.add_option('--bootstrap-scos', dest='bootstrap_scos',
                     metavar='BOOTSTRAP_SCOS',
                     help='scos for bootstap coverage report. '
                     'Use xcov to assess coverage of its own testsuite. '
                     'Only supported on x86-linux. '
                     'Note that it disables the use of valgrind.')
        m.add_option('--board', dest='board', metavar='BOARD',
                     help='Specific target board to exercize.')
        m.add_option('--RTS', dest='RTS', metavar='RTS',
                     help='RTS library to use, mandatory for BSP support')
        m.add_option('--kernel', dest='kernel', metavar='KERNEL',
                     help='KERNEL to pass to gnatcov run in addition to exe')
        m.add_option(
            '--toolchain', dest='toolchain', metavar='TOOLCHAIN',
            default="", help='Use toolchain in the provided path value')
        m.parse_args()

        self.enable_valgrind = (
            m.options.enable_valgrind and m.options.bootstrap_scos == None)

        if not m.options.RTS:
            m.error ("RTS argument missing, mandatory for BSP selection")

        # Determine the test filtering regexp

        m.options.run_test = m.args[0] if m.args else ""

        # --cargs "" should be kept semantically equivalent to absence
        # of --cargs at all, and forcing a string allows simpler code
        # downstream.

        [m.options.__dict__.__setitem__ (opt, "")
         for opt in ("cargs%s" % ext
                     for ext in [""] + ["_%s" % lang for lang in QLANGUAGES])
         if m.options.__dict__[opt] == None]

        return m.options

    # ---------------------
    # -- setup_toolchain --
    # ---------------------

    def setup_toolchain (self, prefix):

        """If a PREFIX is provided, adjust PATH to have PREFIX/bin ahead in
        PATH after sanity checking that it contains at least a couple of
        programs we'll need (e.g. <target>-gcc and gprbuild)."""

        # If we don't have a PREFIX to enforce, stop here. Otherwise, proceed
        # to update PATH for that.

        if not prefix:
            return

        # Sanity check that <toolchain>/bin contains at least
        # a couple of binaries we'll need

        bindir = os.path.join (prefix, "bin")
        exeext = self.env.host.os.exeext

        def check_for (pgm):
            if (not os.path.exists (os.path.join (bindir, pgm))):
                raise FatalError ('Missing "%s" in "%s"' % (pgm, bindir))

        [check_for (p+exeext) for p in (
                self.env.target.triplet+"-gcc", "gprbuild")]

        # Adjust PATH to place <bindir> ahead so that the tests we
        # spawn use it.

        self.env.add_search_path(
            env_var = 'PATH', path = bindir, append = False)

# ==============
# == TestCase ==
# ==============

class TestCase(object):

    # Index to assign to the next instance of this class
    index = 0

    def __init__(self, filename, trace_dir=None):
        """Create a new TestCase for the given filename. If trace_dir
        is specified, save the bootstrap traces there."""
        self.testdir      = os.path.dirname(filename)
        self.filename     = filename
        self.expected_out = None
        self.opt          = None
        self.trace_dir    = trace_dir

        self.index        = TestCase.index
        TestCase.index += 1

    def __lt__(self, right):
        """Use filename alphabetical order"""
        return self.filename < right.filename

    # ---------------------------------
    # -- Testcase options and status --
    # ---------------------------------

    def __try_killcmd (self, cmd):
        """See if CMD applies to this testcase according to test.opt.
        Set self.cmd to the corresponding text for GAIA reports."""
        value = self.opt.get_value (cmd)
        self.killcmd = (
            None if value is None
            else "%s:%s" % (cmd, value)
            )

    def parseopt(self, tags):
        """Parse the test.opt with the given tags"""
        test_opt = os.path.join(self.testdir, 'test.opt')

        self.opt = (
            OptFileParse(tags, test_opt) if os.path.exists(test_opt)
            else None
            )

        self.expected_out = self.getopt('out', 'test.out')

        self.killcmd = None
        if self.opt:
            [self.__try_killcmd (c) for c in ('DEAD', 'SKIP') if not self.killcmd]

    def getopt(self, key, default=None):
        """Get the value extracted from test.opt that correspond to key
        If key is not found. Returns default.
        """
        return (
            self.opt.get_value(key, default_value=default) if self.opt
            else default
            )

    # ---------------------------
    # -- Testcase output files --
    # ---------------------------

    def outf(self):
        """Return the name of the file where outputs of the provided
        test object should go. Same location as the test source script,
        with same name + a .out extra suffix extension."""
        return os.path.join(os.getcwd(), self.filename + '.out')

    def logf(self):
        """Similar to outfile, for the file where logs of the commands
        executed by the provided test object should go."""
        return os.path.join(os.getcwd(), self.filename + '.log')

    def diff(self):
        """Similar to outf, for the file where diffs of the provided test
        object should go."""
        return os.path.join(os.getcwd(), self.filename + '.err')

    def qdaf(self):
        return qdaf_in(self.testdir)

    # -----------------------------
    # -- Testcase identification --
    # -----------------------------

    def rname(self):
        """A unique representative name for TEST, to be used in GAIA web URLs."""

        # Start from the path to test.py, remove the value-less parts and
        # replace slashes which would introduce problematic articial layers in
        # URLs eventually.

        filename = self.filename.replace('test.py', '')
        if filename.startswith('./'):
            filename = filename[2:]

        return filename.strip('/').replace('/', '-')

    def qualif_levels(self):
        """List of qualification levels to which SELF applies"""

        # Check which QLEVEL subtrees would match ...
        return [
            qlevel for qlevel in QLEVEL_INFO
            if re.search (QLEVEL_INFO[qlevel].subtrees, self.testdir)]

    def lang(self):
        """The language specific subtree SELF pertains to"""
        for lang in QLANGUAGES:
            if self.testdir.find ("%s/%s/" % (QROOTDIR, lang)) != -1:
                return lang
        return None

# ======================
# == Global functions ==
# ======================

def _quoted_argv():
    """Return a list of command line options used to when invoking this
    script.  The different with sys.argv is that the first entry (the
    name of this script) is stripped, and that arguments that have a space
    in them get quoted.  The goal is to be able to copy/past the quoted
    argument in a shell and obtained the desired effect."""
    quoted_args = []
    for arg in sys.argv[1:]:
        if ' ' in arg:
           eq_idx = arg.find('=')
           if eq_idx < 0:
               quoted_arg = "'" + arg + "'"
           else:
               quoted_arg = arg[:eq_idx] + "='" + arg[eq_idx + 1:] + "'"
        else:
           quoted_arg = arg
        quoted_args.append(quoted_arg)
    return quoted_args

# =================
# == script body ==
# =================

# Instanciate and run a TestSuite object ...

if __name__ == "__main__":
    suite = TestSuite()
    suite.run()

    if suite.options.qualif_level:
        QDreport(options=suite.options, qdreg=suite.qdreg)
