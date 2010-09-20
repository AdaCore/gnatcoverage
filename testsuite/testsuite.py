#!/usr/bin/env gnatpython

"""./testsuite.py [OPTIONS] [TSET_PATH]

Run the couverture testsuite

To run tests whose relative path to test.py match a provided regexp
   ./testsuite.py I401-009
   ./testsuite.py ./tests/I4

See ./testsuite.py -h for more help
"""

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import mkdir, rm, ln
from gnatpython.main import Main
from gnatpython.mainloop import MainLoop

from gnatpython.optfileparser import OptFileParse
from gnatpython.reports import ReportDiff

from glob import glob

import logging
import os
import re
import sys

DEFAULT_TIMEOUT = 600

# A dictionary whose keys are the recognized values for the --qualif-level
# command-line switch, and whose values are the corresponding xcov --level
# value.
QUALIF_TO_XCOV_LEVEL = \
{ "doA" : "stmt+mcdc",
  "doB" : "stmt+decision",
  "doC" : "stmt"
}

# Set to True if the tests should not be run under valgrind control.
disable_valgrind=False

def main():
    """Run the testsuite and generate reports"""
    # Parse command lines options
    options = __parse_options()

    # Add current directory in PYTHONPATH (to find test_utils.py)
    env = Env()
    env.add_search_path('PYTHONPATH', os.getcwd())

    # Setup log directories
    mkdir('output')
    if options.bootstrap:
        trace_dir = os.path.join(os.getcwd(), 'output/traces')
        rm(trace_dir, recursive=True)
        mkdir(trace_dir)
    else:
        trace_dir = None

    # Generate the discs list for test.opt parsing
    common_discs = ['ALL', env.target.platform]

    # Compute the test list. Use ./ in paths to maximize possible regexp
    # matches, in particular to allow use of command-line shell expansion
    # to elaborate the expression.

    scovDir = "./Qualif/Ada/"
    collDir = "./tests/"

    non_dead_list, dead_list = generate_testcase_list(
        filter_list(collDir+'*/test.py', options.run_test)
        + filter_list(collDir+'*/*/test.py', options.run_test)
        + filter_list(collDir+'*/*/*/test.py', options.run_test)
        + filter_list(collDir+'*/*/*/*/test.py', options.run_test)
        + filter_list(scovDir+'*/test.py', options.run_test)
        + filter_list(scovDir+'*/*/test.py', options.run_test)
        + filter_list(scovDir+'*/*/*/test.py', options.run_test)
        + filter_list(scovDir+'*/*/*/*/test.py', options.run_test)
        + filter_list(scovDir+'*/*/*/*/*/test.py', options.run_test)
        , common_discs, trace_dir)

    # Main loop :
    #   - run all the tests
    #   - collect the test results
    #   - generate the report

    # First report all dead tests
    with open(os.path.join('output', 'results'), 'w') as result_f:
        for test in dead_list:
            result_f.write('%s:DEAD:' % test.filename)

    # Compute targetprefix, prefix to designate target specific versions of
    # command line tools (a-la <prefix>-gnatmake) and expected as the --target
    # argument of other command line tools such as gprbuild or gprconfig.

    targetprefix=env.target.triplet

    # Build a gprbuild configuration file for the testsuite as a whole. Doing
    # it here both factorizes the work for all testcases and prevents cache
    # effects if PATH changes between testsuite runs.

    Run(['gprconfig', '--batch',
         '--config=Ada,,zfp',  '--config=C',  '--config=Asm',
         '--target=%s' % targetprefix, '-o', 'suite.cgpr' ],
        output='output/gprconfig.out')

    # Build support library as needed
    Run(['make', '-C', 'support', '-f', 'Makefile.libsupport',
         'TARGET=%s' % targetprefix],
        output='output/build_support.out')

    # Then run all non dead tests
    MainLoop(non_dead_list,
             run_testcase,
             gen_collect_result(options.diffs),
             options.jobs)

    # Human readable report (rep file)
    ReportDiff('output', options.old_res).txt_image('rep_couverture')

def filter_list(pattern, run_test=""):
    """Compute the list of test matching pattern

    If run_test is not null, run only tests containing run_test
    """
    test_list = glob(pattern)
    if not run_test:
        return test_list
    else:
        return [t for t in test_list if re.search(run_test,t)]

def generate_testcase_list(test_list, discs, trace_dir):
    """Generate the testcase list

    Returns two sorted list:
        - the non dead test list (to be run in the mainloop)
        - the dead test list (not to be run)
    """
    dead_list = []
    non_dead_list = []
    for test in test_list:
        tc = TestCase(test, trace_dir)
        tc.parseopt(discs)
        if tc.is_dead():
            dead_list.append(tc)
        else:
            non_dead_list.append(tc)

    # Sort lists
    non_dead_list.sort()
    dead_list.sort()
    return (non_dead_list, dead_list)

class TestCase(object):
    """Creates a TestCase object.

    Contains the result fo the test.opt parsing
    """
    def __init__(self, filename, trace_dir=None):
        """Create a new TestCase for the given filename. If trace_dir
        is specified, save the bootstrap traces there."""
        self.testdir      = os.path.dirname(filename)
        self.filename     = filename
        self.expected_out = None
        self.opt          = None
        self.trace_dir    = trace_dir

    def __lt__(self, right):
        """Use filename alphabetical order"""
        return self.filename < right.filename

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

def ofile_for(test):
    """Return the name of the file where outputs of the provided
    test object should go. Same location as the test source script,
    with same name + a .out extra suffix extension."""
    return os.path.join(os.getcwd(), test.filename + '.out')

def odiff_for(test):
    """Return the name of the file where diffs of the provided test
    object should go.
    """
    return os.path.join(os.getcwd(), test.filename + '.err')

def olog_for(test):
    """Similar to ofile_for, for the file where logs of the commands
    executed by the provided test object should go."""
    return os.path.join(os.getcwd(), test.filename + '.log')

def rname_for(test):
    """Returns an uniq name for each test"""
    filename = test.filename.replace('test.py', '')
    if filename.startswith('./'):
        filename = filename[2:]
    return filename.strip('/').replace('/', '-')

def rdiff_for(test):
    """Returns path to diff file in the output directory

    This file is used to generate report and results files
    """
    filename = test.filename.replace('test.py', '')
    if filename.startswith('./'):
        filename = filename[2:]
    filename = filename.strip('/').replace('/', '-')
    return os.path.join('output', filename + '.out')

test_index = 0

def run_testcase(test, _job_info):
    """Run a single test

    If limit is not set, run rlimit with DEFAULT_TIMEOUT
    """
    global test_index
    logging.debug("Running " + test.testdir)
    timeout = test.getopt('limit')
    if timeout is None:
        timeout = DEFAULT_TIMEOUT

    output = ofile_for(test)
    outlog = olog_for(test)
    outdiff = odiff_for(test)

    testcase_cmd = [sys.executable,
                    test.filename,
                    '--report-file=' + output,
                    '--log-file=' + outlog,
                    '--target', Env().target.platform,
                    '--timeout', str(timeout)]
    if disable_valgrind:
        testcase_cmd.append('--disable-valgrind')
    if test.trace_dir is not None:
        test_index += 1
        test_trace_dir = os.path.join(test.trace_dir, str(test_index))
        mkdir(test_trace_dir)
        testcase_cmd.append('--trace_dir=%s' % test_trace_dir)
    if Env().main_options.qualif_cargs is not None:
        testcase_cmd.append('--qualif-cargs=%s'
                            % Env().main_options.qualif_cargs)
    if Env().main_options.qualif_level is not None:
        qualif_xcov_level=QUALIF_TO_XCOV_LEVEL[Env().main_options.qualif_level]
        testcase_cmd.append('--qualif-xcov-level=%s' % qualif_xcov_level)
    return Run(testcase_cmd, output=outdiff,
               bg=True, timeout=int(timeout) + DEFAULT_TIMEOUT)

def gen_collect_result(show_diffs=False):
    """Returns the collect_result function"""
    # success - xfail status dict
    status_dict = {True: {True: 'UOK', False: 'OK'},
                   False: {True: 'XFAIL', False: 'FAILED'}}

    def collect_result(test, _process, _job_info):
        """Collect a test result"""
        xfail_comment = test.getopt('xfail', None)
        xfail = xfail_comment is not None

        failed_comment = test.getopt('failed', None)

        # Avoid \ in filename for the final report
        test.filename = test.filename.replace('\\', '/')

        output = ofile_for(test)
        test_out = ""
        if os.path.exists(output):
            f = open(output)
            test_out = f.read()
            if re.search("==== PASSED ============================.",
                         test_out):
                success = True
            else:
                success = False
                f.seek(0)
                f.close()
        else:
            success = False
        if not success:
            rdiff = rdiff_for(test)
            if os.path.exists(rdiff):
                rm(rdiff)
            ln(odiff_for(test), rdiff)
            diff_fd = open(odiff_for(test))
            diff = diff_fd.read()
            diff_fd.close()

        status = status_dict[success][xfail]
        if xfail_comment:
            logging.info("%-60s %s (%s)" %
                         (test.filename, status, xfail_comment))
        elif failed_comment:
            logging.info("%-60s %s (%s)" %
                         (test.filename, status, failed_comment))
        else:
            logging.info("%-60s %s" % (test.filename, status))

        with open(os.path.join('output', 'results'), 'w') as result_f:
            if not success:
                if xfail_comment:
                    result_f.write('%s:%s:%s' %
                            (rname_for(test), status, xfail_comment.strip('"')))
                elif failed_comment:
                    result_f.write('%s:%s:%s' %
                            (rname_for(test), status, failed_comment.strip('"')))
                else:
                    result_f.write('%s:%s:' % (rname_for(test), status))
                if show_diffs and not xfail and not failed_comment:
                    logging.info(diff)
            else:
                result_f.write('%s:%s:' % (rname_for(test), status))

    return collect_result

def __parse_options():
    """Parse command lines options"""
    global disable_valgrind

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
                 help='Force the qualification context level to CONTEXT_LEVEL '
                      'instead of deducing it from the test category.')
    m.add_option('--bootstrap', dest="bootstrap",
                 action='store_true', default=False,
                 help='Use xcov to measure the coverage of its own testsuite.'
                      'Only supported on x86-linux.'
                      'Note that it disables the use of valgrind.')
    m.parse_args()
    disable_valgrind = m.options.disable_valgrind or m.options.bootstrap

    if m.args:
        # Run only tests matching provided regexp
        m.options.run_test = m.args[0]
        logging.info("Running only test matching '%s'" % m.options.run_test)
    else:
        m.options.run_test = ""

    return m.options

if __name__ == "__main__":
    main()
