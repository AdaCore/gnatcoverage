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

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import mkdir, rm, ln, find
from gnatpython.main import Main
from gnatpython.mainloop import MainLoop

from gnatpython.optfileparser import OptFileParse
from gnatpython.reports import ReportDiff

from glob import glob

import logging, os, re, sys

from SUITE.cutils import contents_of, re_filter

DEFAULT_TIMEOUT = 600

# A dictionary whose keys are the recognized values for the --qualif-level
# command-line switch, and whose values are the corresponding xcov --level
# value.
QUALIF_TO_XCOV_LEVEL = \
{ "doA" : "stmt+mcdc",
  "doB" : "stmt+decision",
  "doC" : "stmt"
}

# ===============
# == TestSuite ==
# ===============

class TestSuite:

    def __init__(self):
        """Prepare the testsuite run: parse options, compute and dump
        discriminants, compute lists of dead/non-dead tests, run gprconfig and
        build the support library for the whole series of tests to come"""

        # Parse command lines options

        # Set to True if the tests should not be run under valgrind control.
        self.disable_valgrind = False

        self.options = self.__parse_options()

        # Add current directory in PYTHONPATH, allowing TestCases to find the
        # SUITE and SCOV packages
        self.env = Env()
        self.env.add_search_path('PYTHONPATH', os.getcwd())

        # Setup log directories
        mkdir('output')
        if self.options.bootstrap:
            self.trace_dir = os.path.join(os.getcwd(), 'output/traces')
            rm(self.trace_dir, recursive=True)
            mkdir(self.trace_dir)
        else:
            self.trace_dir = None

        # Generate the discs list for test.opt parsing
        discs = self.discriminants()

        # Dump the list of discriminants in a file.  We can then use that file
        # to determine which discriminants were set during a particular run.
        with open(os.path.join('output', 'discs'), 'w') as fd:
            fd.write(" ".join(discs) + "\n")

        # Dump useful information about this run in a file.  This file can be
        # used as a testsuite report header, allowing a review to determine
        # immediately how the testsuite was run to get those results.  For
        # now, we only provide the command-line switches.
        with open(os.path.join('output', 'comment'), 'w') as fd:
            fd.write("Options: " + " ".join(_quoted_argv()) + "\n")

        # Compute the test list. Use ./ in paths to maximize possible regexp
        # matches, in particular to allow use of command-line shell expansion
        # to elaborate the expression.

        self.non_dead_list, self.dead_list = self.generate_testcase_list(
            re_filter(find (root=".", pattern="test.py", follow_symlinks=True),
                      self.options.run_test), discs)

        # Report all dead tests
        with open(os.path.join('output', 'results'), 'w') as fd:
            [fd.write('%s:DEAD:\n' % dt.filename) for dt in self.dead_list]

        # Compute targetprefix, prefix to designate target specific versions
        # of command line tools (a-la <prefix>-gnatmake) and expected as the
        # --target argument of other command line tools such as gprbuild or
        # gprconfig.

        targetprefix = self.env.target.triplet

        # Build a gprbuild configuration file for the testsuite as a
        # whole. Doing it here both factorizes the work for all testcases and
        # prevents cache effects if PATH changes between testsuite runs.

        # When --rtsgpr is provided (and non empty), e.g. for Ravenscar,
        # assume it controls the necessary --RTS flags to pass. Otherwise,
        # assume we are targetting zfp and configure to pass --RTS=zfp by
        # default

        defrts = "zfp" if not self.options.rtsgpr else ""

        Run(['gprconfig', '--batch',
             '--config=Ada,,%s' % defrts,  '--config=C',  '--config=Asm',
             '--target=%s' % targetprefix, '-o', 'suite.cgpr' ],
            output='output/gprconfig.out')

        # Build support library as needed
        targetargs = ["TARGET=%s" % targetprefix]
        if self.env.main_options.board:
            targetargs.append ("BOARD=%s" % self.env.main_options.board)
        Run(['make', '-C', 'support', '-f', 'Makefile.libsupport']+targetargs,
            output='output/build_support.out')

    # -------------------------------
    # -- Discriminant computations --
    # -------------------------------

    def discriminants (self):
        """Full set of discriminants that apply to this test"""
        return self.base_discriminants() \
            + self.qualif_cargs_discriminants() \
            + self.ravenscar_discriminants()

    def base_discriminants(self):
        return ['ALL'] + self.env.discriminants

    def qualif_cargs_discriminants(self):
        """Compute a list of discriminants (string) for each switch passed
        in the --qualif-cargs command-line option.  The format of each
        discriminant is a follow: QUALIF_CARGS_<X> where <X> is the switch
        stripped of its leading dashes.  For instance, if this testsuite
        is called with --qualif-cargs='-gnatp -O1', then this function should
        return ['QUALIF_CARGS_gnatp', 'QUALIF_CARGS_O1'].

        Return an empty list if --qualif-cargs was not used.
        """
        if not self.env.main_options.qualif_cargs:
            return []
        discs = []
        for arg in self.env.main_options.qualif_cargs.split():
            discs.append("QUALIF_CARGS_" + arg.lstrip('-'))
        return discs

    def ravenscar_discriminants(self):
        """Compute a list of discriminants (string) to reflect the use of a
        Ravenscar base runtime library, as conveyed by the base gpr file to
        extend, provided with the --rtsgpr command-line option.
        """
        if not self.env.main_options.rtsgpr:
            return []
        discs = []
        if re.search ("ravenscar", self.env.main_options.rtsgpr):
            discs.append("RTS_RAVENSCAR")
        return discs

    # ----------------------------
    # -- generate_testcase_list --
    # ----------------------------

    def generate_testcase_list(self, test_list, discs):
        """Compute and return two sorted list:
            - the non-dead test list (to be run in the mainloop)
            - the dead test list (not to be run)
        """
        dead_list = []
        non_dead_list = []
        for test in test_list:
            tc = TestCase(test)
            tc.parseopt(discs)
            if tc.is_dead():
                dead_list.append(tc)
            else:
                non_dead_list.append(tc)

        # Sort lists
        non_dead_list.sort()
        dead_list.sort()
        return (non_dead_list, dead_list)

    # ---------
    # -- run --
    # ---------

    def run (self):

        # Main loop : run all the tests and collect the test results,
        # then generate the human readable report

        MainLoop(self.non_dead_list,
                 self.run_testcase,
                 self.collect_result,
                 self.options.jobs)

        ReportDiff('output', self.options.old_res).txt_image('rep_couverture')

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

        [os.remove (f) for f in (outf, logf, diff) if os.path.exists(f)]

        testcase_cmd = [sys.executable,
                        test.filename,
                        '--report-file=' + outf,
                        '--log-file=' + logf,
                        '--target', self.env.target.platform,
                        '--timeout', str(timeout)]
        if self.disable_valgrind:
            testcase_cmd.append('--disable-valgrind')
        if self.trace_dir is not None:
            test_trace_dir = os.path.join(test.trace_dir, str(test.index))
            mkdir(test_trace_dir)
            testcase_cmd.append('--trace_dir=%s' % test_trace_dir)

        # Propagate our command line arguments as testcase options.
        #
        # Beware that we're not using 'is not None' on purpose, to prevent
        # propagating empty arguments.

        mopt = self.env.main_options
        if mopt.qualif_cargs:
            testcase_cmd.append('--qualif-cargs=%s' % mopt.qualif_cargs)

        if mopt.qualif_level:
            qualif_xcov_level = QUALIF_TO_XCOV_LEVEL[mopt.qualif_level]
            testcase_cmd.append('--qualif-xcov-level=%s' % qualif_xcov_level)

        if mopt.board:
            testcase_cmd.append('--board=%s' % mopt.board)

        if mopt.rtsgpr:
            testcase_cmd.append('--rtsgpr=%s' % mopt.rtsgpr)

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

        # Compute a few useful facts: Whether the test passed or failed,
        # if it was xfailed, with what comment, what was the error log when
        # the test failed, ...

        # Compute the actual execution status, what really happened
        # independant from what was expected;

        output = test.outf()
        if os.path.exists(output):
            f = open(output)
            success = (
                True if re.search(
                    "==== PASSED ============================.", f.read())
                else False)
            f.close()
        else:
            success = False

        # If the execution failed, arrange to get a link to the err log
        # where the infrastructure expects it (typically not in the test
        # dedicated subdirectory where the original log resides)

        if not success:
            odiff = self.odiff_for(test)
            if os.path.exists(odiff):
                rm(odiff)
            ln(test.diff(), odiff)

        # Compute the status of this test (OK, UOK, FAILED, XFAIL) from
        # the combination of its execution success and a possible failure
        # expectation

        xfail_comment = test.getopt('xfail', None)
        xfail = xfail_comment is not None

        failed_comment = test.getopt('failed', None)

        status_dict = {
            # XFAIL?   PASSED => status   !PASSED => status
              True:    {True:    'UOK',    False:    'OK'},
              False:   {True:    'XFAIL',  False:    'FAILED'}}

        status = status_dict[success][xfail]

        # Now log and populate "results" file

        # Avoid \ in filename for the final report
        test.filename = test.filename.replace('\\', '/')

        if xfail_comment:
            logging.info("%-60s %s (%s)" %
                         (test.filename, status, xfail_comment))
        elif failed_comment:
            logging.info("%-60s %s (%s)" %
                         (test.filename, status, failed_comment))
        else:
            logging.info("%-60s %s" % (test.filename, status))

        with open(os.path.join('output', 'results'), 'a') as result_f:
            if not success:
                if xfail_comment:
                    result_f.write(
                        '%s:%s:%s\n' %
                        (test.rname(), status, xfail_comment.strip('"')))
                elif failed_comment:
                    result_f.write(
                        '%s:%s:%s\n' %
                        (test.rname(), status, failed_comment.strip('"')))
                else:
                    result_f.write('%s:%s:\n' % (test.rname(), status))
                if self.options.diffs and not xfail and not failed_comment:
                    logging.info(contents_of (test.diff()))
            else:
                result_f.write('%s:%s:\n' % (test.rname(), status))

    def odiff_for(self, test):
        """Returns path to diff file in the suite output directory.  This file
        is used to generate report and results files."""

        filename = test.filename.replace('test.py', '')
        if filename.startswith('./'):
            filename = filename[2:]
        filename = filename.strip('/').replace('/', '-')
        return os.path.join('output', filename + '.out')

    # -------------------
    # -- parse_options --
    # -------------------

    def __parse_options(self):
        """Parse command lines options"""

        m = Main(add_targets_options=True)
        m.add_option('--diffs', dest='diffs', action='store_true',
                     default=False, help='show diffs on stdout')
        m.add_option('--disable-valgrind', dest='disable_valgrind',
                     action='store_true', default=False,
                     help='disable the use of valgrind when running each test')
        m.add_option('-j', '--jobs', dest='jobs', type='int',
                     metavar='N', default=1, help='Allow N jobs at once')
        m.add_option("--old-res", dest="old_res", type="string",
                        help="Old testsuite.res file")
        m.add_option('--qualif-cargs', dest='qualif_cargs', metavar='ARGS',
                     help='Additional arguments to pass to the compiler '
                          'when building the test programs.')
        m.add_option('--qualif-level', dest='qualif_level',
                     type="choice", choices=QUALIF_TO_XCOV_LEVEL.keys(),
                     metavar='CONTEXT_LEVEL',
                     help='Force the qualification context to CONTEXT_LEVEL '
                          'instead of deducing it from the test category.')
        m.add_option('--bootstrap', dest="bootstrap",
                     action='store_true', default=False,
                     help='Use xcov to assess coverage of its own testsuite.'
                          'Only supported on x86-linux.'
                          'Note that it disables the use of valgrind.')
        m.add_option('--board', dest='board', metavar='BOARD',
                     help='Specific target board to exercize.')
        m.add_option('--rtsgpr', dest='rtsgpr', metavar='RTSGPR',
                     help='RTS .gpr to extend.')
        m.parse_args()

        self.disable_valgrind = (
            m.options.disable_valgrind or m.options.bootstrap)

        if m.args:
            # Run only tests matching provided regexp
            m.options.run_test = m.args[0]
            logging.info("Running tests matching '%s'" % m.options.run_test)
        else:
            m.options.run_test = ""

        return m.options

# ==============
# == TestCase ==
# ==============

class TestCase(object):

    # Index to assign to the next instance of this class
    index = 0

    def __init__(self, filename):
        """Create a new TestCase for the given filename. If trace_dir
        is specified, save the bootstrap traces there."""
        self.testdir      = os.path.dirname(filename)
        self.filename     = filename
        self.expected_out = None
        self.opt          = None

        self.index        = TestCase.index
        TestCase.index += 1

    def __lt__(self, right):
        """Use filename alphabetical order"""
        return self.filename < right.filename

    # ---------------------------------
    # -- Testcase options and status --
    # ---------------------------------

    def parseopt(self, tags):
        """Parse the test.opt with the given tags"""
        test_opt = os.path.join(self.testdir, 'test.opt')
        if os.path.exists(test_opt):
            self.opt = OptFileParse(tags, test_opt)
        self.expected_out = self.getopt('out', 'test.out')

    def getopt(self, key, default=None):
        """Get the value extracted from test.opt that correspond to key

        If key is not found. Returns default.
        """
        if self.opt is None:
            return default
        else:
            return self.opt.get_value(key, default_value=default)

    def is_dead(self):
        """Returns True if the test is DEAD"""
        if self.opt is None:
            return False
        else:
            return self.opt.is_dead

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

    def tcof(self):
        return os.path.join(os.getcwd(), "tc.pkl")

    # -----------------------------
    # -- Testcase identification --
    # -----------------------------

    def rname(test):
        """A unique representative name for TEST"""

        filename = test.filename.replace('test.py', '')
        if filename.startswith('./'):
            filename = filename[2:]
        return filename.strip('/').replace('/', '-')

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
