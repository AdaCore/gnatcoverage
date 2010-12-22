#!/usr/bin/env gnatpython

"""test utils

This module is imported by all testcases. It:

- Exposes a single instance of a Test class to offer command line and test
  status management facilities,

- Provides a set of useful functions to abstract details of common
  operations away.

- Exposes a number of elaborate facilities to help the development of
  source coverage tests.

You should never call this module directly. To run a single testcase, use
 ./testsuite.py NAME_OF_TESTCASE
"""

from gnatpython.ex import Run, PIPE
from gnatpython.env import Env
from gnatpython.main import Main

from gnatpython.fileutils import cd, rm, which, diff, touch, mkdir, ls, find

import os
import re
import sys

# Move to test directory
ROOT_DIR = os.getcwd()
TEST_DIR = os.path.dirname(sys.modules['__main__'].__file__)
cd(TEST_DIR)

# The Qualif directory, where all tests used for qualification are located.
QUALIF_DIR = os.path.join(ROOT_DIR, "Qualif")

env = Env()

# Append .exe on windows for native tools
VALGRIND = 'valgrind' + env.host.os.exeext
GPRBUILD = 'gprbuild' + env.host.os.exeext
GPRCLEAN = 'gprclean' + env.host.os.exeext
XCOV     = 'xcov' + env.host.os.exeext

# ***************************************************************************
#         FatalError Exception, to raise when processing has to stop
# ***************************************************************************

class FatalError(Exception):
    def __init__(self,comment,output=None):
        if output != None:
            comment += '. Output was:\n'+contents_of(output)
        self.comment = comment

    def __str__(self):
        return self.comment

class ReportOutput(object):
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
        self.report_fd = open(report_file, "w")
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

    def close(self):
        """Close the file descriptor for our report file.
        """
        self.report_fd.close()

# ***************************************************************************
#                       Test class and single instance
# ***************************************************************************

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
        """Initialize the instance: parse command line options, reset
        the failures counter and precompute gprbuild options we'll have
        to pass on every call to convey configuration options.
        """
        self.options = self.cmdline_options()
        self.n_failed = 0
        self.report = ReportOutput(self.options.report_file)
        self.current_test_index = 0

        self.gprconfoptions = ['-v', '--config=%s' % ROOT_DIR+'/suite.cgpr',
                               '-XTARGET=%s' % env.target.triplet]
        if self.options.board:
            self.gprconfoptions.append ('-XBOARD=%s' % self.options.board)

    # -------------
    # -- cleanup --
    # -------------

    def cleanup(self, project):
        """Cleanup possible remnants of previous builds."""

        Run([GPRCLEAN, "-P%s" % project] + self.gprconfoptions)
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
        # Perform a flush of the output, in case we forgot to do so earlier.
        # This has no effect if the flush was performed, but might provide
        # valuable output if not.
        self.flush()
        self.report.close()

    # -----------------------------
    # -- Test options management --
    # -----------------------------

    def cmdline_options(self):
        """Return an options object to represent the command line options"""
        main = Main(require_docstring=False, add_targets_options=True)
        main.add_option('--timeout', dest='timeout', type=int,
                        default=None)
        main.add_option('--disable-valgrind', dest='disable_valgrind',
                        action='store_true', default=False)
        main.add_option('--trace_dir', dest='trace_dir', metavar='DIR',
                        help='Traces location. No bootstrap if not specified.',
                        default=None)
        main.add_option('--report-file', dest='report_file', metavar='FILE',
                        help='The filename where to store the test report '
                             '[required]')
        main.add_option('--qualif-cargs', dest='qualif_cargs', metavar='ARGS',
                        help='Additional arguments to pass to the compiler '
                             'when building the test programs.')
        main.add_option('--qualif-xcov-level', dest='qualif_xcov_level',
                        metavar='CONTEXT_LEVEL',
                        help='For qualification tests, force the context '
                             'level to CONTEXT_LEVEL instead of deducing it '
                             'from the test category.')
        main.add_option('--board', dest='board', metavar='BOARD',
                        help='Specific target board to exercize')
        main.parse_args()
        if main.options.report_file is None:
            # This is a required "option" which is a bit self-contradictory,
            # but it's easy to do it that way.
            main.error("The report file must be specified with --report-file")
        return main.options

    def support_dir(self):
        return os.path.join (ROOT_DIR, 'support')

# Instantiate a Test object for the individual test module that
# imports us.

thistest = Test ()

def qualification_test_p():
    """Return true if the current test is test used for qualification.
    """
    # A test is a qualification test if its path starts with QUALIF_DIR.
    # Strictly speaking, we should also verify that it is followed by
    # a directory separator.  But, in practice, just verifying that
    # it starts with QUALIF_DIR is sufficient for our purposes.
    return os.path.abspath(TEST_DIR).startswith(QUALIF_DIR)

# ***************************************************************************
#                                User functions
# ***************************************************************************

# -----------------
# -- contents_of --
# -----------------
def contents_of(filename):
    """Return contents of file FILENAME"""
    f = open(filename)
    contents = f.read()
    f.close()

    return contents

# --------------
# -- lines_of --
# --------------
def lines_of(filename):
    """Return contents of file FILENAME as a list of lines"""
    f = open(filename)
    contents = f.readlines()
    f.close()

    return contents

# -------------
# -- to_list --
# -------------
def to_list(blob):
    """Turn input BLOB into a list if it isn't already. Handle None
       and whitespace separated strings. Return empty list otherwise."""

    if isinstance (blob, list):
        return blob

    if blob == None:
        return []

    if isinstance (blob, str):
        return blob.split ()

    return []

# ------------------
# -- text_to_file --
# ------------------
def text_to_file(text, filename="tmp.list"):
    """Write TEXT to file FILENAME. Overwrite current contents.
    Return FILENAME."""

    f = open (filename, "w")
    f.write (text)
    f.close ()
    return filename

# ------------------
# -- list_to_file --
# ------------------
def list_to_file(l, filename="tmp.list"):
    """Write list L to file FILENAME, one item per line. Typical use is
       to generate response files. Return FILENAME."""

    return text_to_file ('\n'.join (l) + '\n', filename)

# --------------
# -- gprbuild --
# --------------
def gprbuild(project, gargs=None, cargs=None, largs=None):
    """Cleanup & build the provided PROJECT file using gprbuild, passing
    GARGS/CARGS/LARGS as gprbuild/cargs/largs command-line switches
    (in addition to the switches required by the infrastructure.

    The *ARGS arguments may be either: None, a string containing
    a space-separated list of options, or a list of options."""

    all_gargs = ['-q', '-XSTYLE_CHECKS=', '-p', '-P%s' % project]

    all_gargs += thistest.gprconfoptions
    all_gargs += to_list(gargs)

    all_cargs = to_list(cargs)
    if all_cargs:
        all_cargs.insert(0, '-cargs')

    all_largs = to_list(largs)
    if all_largs:
        all_largs.insert(0, '-largs')

    thistest.cleanup(project)

    ofile = "gprbuild.out"
    p = Run([GPRBUILD] + all_gargs + all_cargs + all_largs,
            output=ofile, timeout=thistest.options.timeout)
    thistest.stop_if (
        p.status != 0, FatalError("gprbuild exit in error", ofile))

# ------------
# -- gprfor --
# ------------
def gprfor(mains, prjid="gen", srcdirs="src"):
    """Generate a simple PRJID.gpr project file to build executables for
    each main source file in the MAINS list, sources in SRCDIRS. Inexistant
    directories in SRCDIRS are ignored. Return the gpr file name.
    """

    mains = to_list(mains)
    srcdirs = to_list(srcdirs)

    # Fetch the support project file template
    template = contents_of (os.path.join (ROOT_DIR, "template.gpr"))

    # Instanciate the template fields. Turn the list of main sources into
    # the proper comma separated sequence of string literals for the Main
    # GPR attribute. Likewise for source dirs, plus filter on existence.

    # The existence check allows widening the set of tentative dirs while
    # preventing complaints from gprbuild about inexistent ones.

    gprmains = ', '.join(['"%s"' % m for m in mains])
    srcdirs  = ', '.join(['"%s"' % d for d in srcdirs if os.path.exists(d)])

    # Remove trailing comma on srcdirs, in case none of the provided ones
    # exists, which would produce an invalid gpr file.

    gprtext = template % {'support_dir': thistest.support_dir(),
                          'prjname': prjid,
                          'srcdirs': srcdirs.rstrip(', '),
                          'gprmains': gprmains}

    # Dump the new contents into the target gpr file and return

    return text_to_file (text = gprtext, filename = prjid + ".gpr")

# --------------------
# -- maybe_valgrind --
# --------------------
def maybe_valgrind(command):
    """Return the input COMMAND list, with 'valgrind -q' prepended if
    valgrind is available and was not disabled through options.
    """
    if (not thistest.options.disable_valgrind) and which(VALGRIND) != '':
        command = [VALGRIND, '-q'] + command
    return command

# ----------
# -- xcov --
# ----------
def xcov(args, out=None, inp=None, register_failure=True):
    """Run xcov with arguments ARGS, timeout control, valgrind control if
    available and enabled, output directed to OUT and failure registration
    if register_failure is True. Return the process status descriptor. ARGS
    may be a list or a whitespace separated string."""

    # make ARGS a list from whatever it is, to allow unified processing
    args = to_list (args)
    retry = 0

    if thistest.options.trace_dir is not None:
        # Bootstrap - run xcov under xcov

        # QEMU's "user" mode is showing some instabilities on x86-linux;
        # see J618-020. So try to re-run if failure. And these instabilities
        # are very visible in 'run' mode; so do the bootstrap for 'coverage'
        # mode only.
        if len (args) > 0 and args[0] == 'coverage':
            thistest.current_test_index += 1
            args = ['run', '-t', 'i386-linux',
                    '-o', os.path.join(thistest.options.trace_dir,
                                       str(thistest.current_test_index)
                                       + '.trace'),
                    which(XCOV), '-eargs'] + args
            retry = 3

    # Execute, check status, raise on error and return otherwise
    p = Run(maybe_valgrind([XCOV]) + args,
            output=out, input=inp, timeout=thistest.options.timeout)
    while p.status != 0 and retry > 0:
        retry -= 1
        p = Run(maybe_valgrind([XCOV]) + args,
                output=out, input=inp, timeout=thistest.options.timeout)
    thistest.stop_if(
        register_failure and p.status != 0,
        FatalError('"xcov ' + ' '.join(args) + '" exit in error', out))
    return p

# ----------
# -- xrun --
# ----------
def xrun(args, out=None):
    """Run <xcov run> with arguments ARGS for the current target."""

    # We special case xcov --run to pass an extra --target option and
    # force a dummy input to prevent mysterious qemu misbehavior when
    # input is a terminal.

    nulinput = "devnul"
    touch(nulinput)

    # On leon-elf, qemu is stopped by generating a double-fault.  This
    # crashes the board and therefore qemu exits with an error message.
    # As this is expected, we don't stop the test because of exit status.

    # Compute our --target argument to xcov run.  If we have a specific
    # target board specified, use that.  Fallback on our general target
    # triplet otherwise.

    if thistest.options.board:
        targetarg = thistest.options.board
    else:
        targetarg = env.target.triplet

    return xcov (['run', '--target=' + targetarg] + to_list(args),
                 inp=nulinput, out=out, register_failure=False)
# -----------
# -- match --
# -----------
def match(pattern, filename, flags=0):
    """Whether regular expression PATTERN could be found in FILENAME"""
    return re.search(pattern, contents_of(filename), flags) is not None

# -------------
# -- differs --
# -------------
def differs (file1, file2):
    """Returns True if the content of the two files are different"""
    diff_string = diff (file1, file2)
    return diff_string != ''

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

# -------------
# -- compile --
# -------------
def compile(source, options):
    """Compile SOURCE with the target compiler, passing OPTIONS on the
    command line."""
    do("%s-gcc -c %s %s"  % (env.target.triplet, options, source))

# ------------
# -- fb_get --
# ------------

def fb_get(dict, key):
    """Get DICT[KEY], falling back to DICT[''] if KEY is not a
    a valid key in DICT"""
    return dict.get(key, dict[""])

# -----------
# -- frame --
# -----------
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
        self.pre  = pre
        self.post = post
        self.char = char

        self.width = 0
        self.lines = text.split('\n')
        [self.register(text) for text in self.lines]


# ================
# ==  MapCheck  ==
# ================
class MapCheck:

    # Helper to compile a set of sources and check clean output of xcov
    # map-routines on a list of alis and objects.

    def __init__(self, sources, options="",
                 objects=None, alis=None, ensure_dcscos=True):
        self.options = "-g -fpreserve-control-flow -gnateS -gnatd.X " + options
        self.sources = to_list(sources)

        # Infer default list of objects and alis from list of sources

        if objects != None:
            self.objects = to_list(objects)
        else:
            self.objects = ["%s.o" % source.split('.')[0]
                            for source in self.sources]

        if alis != None:
            self.alis = to_list(alis)
        else:
            self.alis = ["%s.ali" % source.split('.')[0]
                         for source in self.sources]

        # Compile all the sources

        [compile (source, self.options) for source in self.sources]

        # If requested, check at least one non statement SCO in alis

        if ensure_dcscos:
            [thistest.fail_if (
                    not match ('^C[^S ]', ali, re.MULTILINE),
                    "couldn't find non-statement SCO in %s" % ali)
             for ali in self.alis]

        # Run xcov map-routines and check absence of errors

        mapoutput = do(
            "xcov map-routines -v --scos=@%s %s"
            % (list_to_file(self.alis), " ".join(self.objects)))

        maperrors = [str(m) for m in
                     re.findall("(\*\*\*|\!\!\!)(.*)", mapoutput)]

        thistest.log('\n'.join(maperrors))
        thistest.fail_if(
            maperrors, "expect no map-routines error for %s" % source)

# ***************************************************************************
#                  SOURCE COVERAGE QUALIFICATION - DESIGN KEYS
# ***************************************************************************

# The general idea is to offer services to automate the recurrent parts of
# source coverage testing activities.

# Test CONTEXT and CATEGORY
# -------------------------

# Each test is assigned a CATEGORY, which determines the set of coverage
# criteria it is designed to verify. Orthogonally, each test might be
# exercised in several possible coverage CONTEXTs, typically a target tool
# qualification level.

# The CONTEXT determines which --level argument we pass to xcov coverage.
# The CATEGORY determines the set of outputs we care about.

# For example, we will have categories for statement, decision or
# statement+mcdc testing purposes, which all translate into different sets of
# expectations on the results.

# For a do178 levelA qualification (CONTEXT), we will run all of these tests
# with --level=stmt+mcdc. This will typically produce mcdc related diagnostics
# in !mcdc oriented tests (CATEGORY), which we'll have to ignore.

# We expect the CONTEXT to designate a superset of the CATEGORY.

# Outputs of interest
# -------------------

# We care about two kinds of outputs:
# * the annotated sources produced by xcov --annotate=xcov, and
# * the violations list report produced by xcov --annotate=report
#
# We refer to them as the '=xcov' and the '=report' outputs respectively.

# COVERAGE NOTES (or MARKS) and SLOC SEGMENTS
# -------------------------------------------

# We work over abstractions of expected or emitted coverage indications, which
# we refer to as "coverage marks", or "coverage notes".

# We call "Line" or "L" notes the synthetic signs emitted on a line in the
# =xcov outputs, for example, the '+' or '-' signs in the sample below:
#
#   1 .: function In_Range (X , Min, Max : Integer) return Boolean is
#   2 .: begin
#   3 +:    if X < Min then
#   4 -:       return False;
#   5 +:    elsif X > Max then
#   6 -:       return False;
#   7 .:    else
#   8 +:       return True;
#   9 .:    end if;
#  10 .: end;
#
# Lnote objects convey the kind of information emitted ("line fully covered"
# for '+', "line not covered" for '-') and the line number for which it was
# emitted. They are collected in outer data structures (e.g. dictionaries) to
# bind them with the source file where they were found.

# We call "Report", or "R" notes the indications emitted for slocs in the
# =report outputs, for example:
#
#  in_range.adb:4:7: STATEMENT: not executed
#  in_range.adb:6:7: STATEMENT: not executed
#
# As for Lnotes, Rnote objects convey the kind of indication emitted, the sloc
# for which they were emitted, and are gathered in outer data structures to
# bind them with the source file from which they originate. Different possible
# degrees of precision could be present in the source location designation:
#
#   * for a specific point in the source, e.g. "4:5" for "column 5 on line #4"
#   * for a line as a whole, e.g. "4" or "4:0" for "line #4",
#   * for a line segment, e.g. "4:7-9" for "columns 7 to 9 on line #4"
#   * for a section of source spanning multiple lines,
#     e.g. "3:4-9:7" for "column #4 on line #3 to column #7 on line #9",
#
# To let us evaluate if some reported coverage indication corresponds to an
# expectation, we abstract all these with variations over sloc SECTIONS which
# we can easily check for inclusion within each other. We manipulate general
# sloc SECTIONS, possibly specialized as line SEGMENTs (section with start and
# end on the same line) or full LINEs (segment from first to last column) or
# POINTs (one column segments).

# Expected indications are expressed as comments in single test
# drivers like:
#
# -- test_blob1.adb --
# procedure test_blob1 is
# begin
#   ...
# end;
#
# --# blob.adb       <= start of expectations for unit blob.adb
#
#       o---------- "line regular expression"
#       |    o------ expected Line note for matching lines
#       |    |  o--- set of expected Report notes for matching lines
#       |    |  |
#       v    v  v
# --  /foo/  l- s-   <= for lines matching "-- # foo", expect
#                       a '-' synthetic note on the =xcov line (l-)
#                       a 'statement not covered' =report indication (s-)
#
# --  /bar/  l+ 0    <= for lines matching "-- # bar", expect
#                       a '+' synthetic note on the =xcov line (l+)
#                       no =report indication (0 = empty set)
# --# otherunit.adb
# --  ...
#
# We typically use 'X' in names to denote eXpectation, so will typically
# have Xnote objects to represent the set of expected indications after
# the line patterns were matched against a source.

# ***************************************************************************
#             SOURCE COVERAGE SUPPORT LEVEL 1 : SCOV_helper & al
# ***************************************************************************

# The first facility offered is the SCOV_helper class to allow writing
# things like:
#
#    t = SCOV_helper (
#          drivers="test_blob1.adb", xfile="test_blob1.adb",
#          category="stmt", xcovlevel="stmt")
#    t.run()

# This example is a request to exercise the single test_blob1 driver as a test
# of 'stmt' category, in a context where we'll call xcov with --level='stmt'
# to check that actual results match expectations described in test_blob1.adb
# itself.

# Consolidation tests are supported by passing multiple drivers and
# using an external text file to gather the expectations.

# Symbolic values & names for kinds of coverage notes:
# ----------------------------------------------------

# no code for line (=xcov)
# full coverage for line (=xcov)
# line part of exempted block, 0 deviations (=xcov)
# line part of exempted block, >0 deviations (=xcov)
# line not covered (=xcov)
# line partially covered (=xcov)
# stmt not covered (=report)
# unable to assess precise stmt coverage (=report)
# decision outcome True not covered (=report)
# decision outcome False not covered (=report)
# one decision outcome not covered (=report)
# independent effect of condition not demonstrated (=report)
# exempted block, 0 deviations (=report)
# exempted block, >0 deviations (=report)

lNoCode, lFullCov, \
strictNote, \
lx0, lx1, \
deviationNote, \
lNoCov, lPartCov, \
sNoCov, sPartCov, \
dtNoCov, dfNoCov, dPartCov, dNoCov, cPartCov, \
blockNote, \
xBlock0, xBlock1 = range(18)

# DEVIATION notes are those representing violations of a coverage mandate
# associated with a general criterion (e.g. ).

def deviation_p(nkind):
    return nkind > deviationNote and nkind < blockNote

# POSITIVE notes are those representing a positive statement about a
# coverage mandate, only present in =xcov outputs.

def positive_p(nkind):
    return nkind == lFullCov

# BLOCK notes are those emitted as a single note for a block of code in
# =report outputs,

def block_p(nkind):
    return nkind > blockNote

# STRICT notes are those for which an exact match between reports
# and expectations is required: an expected note should be reported (or err,
# unless the expectation is explicitely tagged weak), and a reported note
# should be expected (or err).

# !STRICT notes should also be reported when expected (or err unless weak
# expectation), but trigger no err when reported eventhough not expected.

def strict_p(nkind):
    return nkind > strictNote


NK_image  = {None: "None",
             lNoCode: "lNoCode", lFullCov: "lFullCov",
             lNoCov: "lNoCov", lPartCov: "lPartCov",
             lx0: "lx0", lx1: "lx1",
             sNoCov: "sNoCov", sPartCov: "sPartCov",
             dtNoCov: "dtNoCov", dfNoCov: "dfNoCov",
             dPartCov: "dPartCov", dNoCov: "dNoCov",
             xBlock0: "xBlock0", xBlock1: "xBlock1",
             cPartCov: "cPartCov"}

# Useful sets of note kinds:
# --------------------------

lNoteKinds = (lNoCode, lNoCov, lPartCov, lFullCov, lx0, lx1)

sNoteKinds = (sNoCov, sPartCov)
dNoteKinds = (dtNoCov, dfNoCov, dPartCov, dNoCov)
cNoteKinds = (cPartCov,)
xNoteKinds = (xBlock0, xBlock1)

# Note kinds that can be associated to one of xcov's message, independantly
# of the context of invocation.
rNoteKinds = sNoteKinds+dNoteKinds+cNoteKinds+xNoteKinds

# Relevant/Possible Line and Report notes for CATEGORY/CONTEXT:
# -------------------------------------------------------------

rp_lnotes_for = { "stmt":     lNoteKinds,
                  "decision": lNoteKinds,
                  "mcdc":     lNoteKinds
                }

rp_rnotes_for = { "stmt":     xNoteKinds+sNoteKinds,
                  "decision": xNoteKinds+sNoteKinds+dNoteKinds,
                  "mcdc":     xNoteKinds+sNoteKinds+dNoteKinds+cNoteKinds
                }

# Note that we do care about exemptions at every level and need to watch out
# for subtle changes in the number of violations exempted when running a given
# test in different contexts (for different target levels).

# Default xcov --level value for every possible test category:
# ------------------------------------------------------------

default_xcovlevels_for = { "stmt":     ["stmt"],
                           "decision": ["stmt+decision"],
                           "mcdc":     ["stmt+uc_mcdc", "stmt+mcdc"]
                         }

# Symbolic strength of each category and context level, to let us determine
# when we're running some test of a given catgeory with a stricter --level
# --------------------------------------------------------------------------

strength = { "stmt": 1,          # category & context level
             "decision" : 2,     # category
             "mcdc" : 3,         # category
             "stmt+decision": 2, # context
             "stmt+mcdc": 3,     # context
             "stmt+uc_mcdc": 3   # context
             }

# ======================================
# == Section, Segment, Line and Point ==
# ======================================

# Sloc first, an internal helper which materializes a line:col coordinate and
# knows to determine if it is past or before another sloc for inclusion check
# purposes.

# The first logical column of a line is numbered 1. Column 0 is used in slocs
# designating a line as a whole. Any specific point on a line is considered to
# be within the line, so past-or-eq the beginning of it, or before-or-eq the
# end of it.

class Sloc:

    def __init__ (self, line, col):
        self.l = line
        self.c = col

    def pastoreq (self, other):
        return (self.l > other.l
                or (self.l == other.l
                    and (self.c >= other.c or other.c == 0)))

    def beforeq (self, other):
        return (self.l < other.l
                or (self.l == other.l
                    and (self.c <= other.c or other.c == 0)))

def Sloc_from(text):
    items = text.split (':', 1)
    return Sloc (
        line = int(items[0]), col = int(items [1]))

# A Section is the association of two slocs to materialize the start
# and the end of a source section, and which knows to determine if it
# is included within another section.

class Section:
    #        ...
    #  l0 -> 3: if a and then b then
    #           ^
    #          c0
    #        4:   val := klunk;
    #        5: else
    #        6:   val := junk;
    #  l1 -> 7: end if;
    #               ^
    #               c1
    #  3:1-7:6

    def __init__ (self, l0, c0, l1, c1):
        self.sloc0 = Sloc (line = l0, col = c0)
        self.sloc1 = Sloc (line = l1, col = c1)

    def within(self, other):
        return (self.sloc0.pastoreq (other.sloc0)
                and self.sloc1.beforeq (other.sloc1))

    def __str__(self):
        return "%d:%d-%d:%d" % (
            self.sloc0.l, self.sloc0.c, self.sloc1.l, self.sloc1.c)

def Section_from(text):
    topitems = text.split ('-', 1)
    subitems0 = topitems[0].split (':', 1)
    subitems1 = topitems[1].split (':', 1)
    return Section (
        l0 = int(subitems0[0]), c0 = int(subitems0[1]),
        l1 = int(subitems1[0]), c1 = int(subitems1[1]))

# A Segment is a Section for which the start and end are known to
# be on the same line.

class Segment (Section):
    def __init__ (self, lno, clo, chi):
        Section.__init__(self, l0 = lno, c0 = clo, l1 = lno, c1 = chi)

    def __str__(self):
        return "%d:%d-%d" % (self.sloc0.l, self.sloc0.c, self.sloc1.c)

def Segment_from(text):
    topitems = text.split (':', 1)
    subitems = topitems[1].split ('-', 1)
    return Segment (
        lno = int(topitems[0]), clo = int(subitems[0]), chi = int(subitems[1]))

# A Line is a Segment spanning for first to last column (arbitrary upper
# bound hardcoded at this stage).

class Line (Segment):
    def __init__ (self, lno):
        Segment.__init__(self, lno = lno, clo = 0, chi = 0)

    def __str__(self):
        return "%d:" % self.sloc0.l

def Line_from(text):
    items = text.split (':', 1)
    return Line (lno = int(items[0]))

# A Point is a Segment for which the start and end columns are identical.

class Point (Segment):
    def __init__ (self, lno, col):
        Segment.__init__(self, lno = lno, clo = col, chi = col)

    def __str__(self):
        return "%d:%d" % (self.sloc0.l, self.sloc0.c)

def Point_from(text):
    items = text.split (':', 1)
    return Point (lno = int(items[0]), col = int(items[1]))

# Search and return a possible Section object with TEXT, specialized
# in accordance with the possible section expression shapes.

def Section_within(text):

    # Search for each possible shape in turn. Beware that the search order is
    # very relevant here.

    m = re.search ("(\d+:\d+-\d+:\d+)", text)
    if m: return Section_from (m.group(1))

    m = re.search ("(\d+:\d+-\d+)", text)
    if m: return Segment_from (m.group(1))

    m = re.search ("(\d+:\d+)", text)
    if m: return Point_from (m.group(1))

    m = re.search ("(\d+:)", text)
    if m: return Line_from (m.group(1))

    return None

# ====================
# == Coverage Notes ==
# ====================

rsNoInterest, rsNotExempted, rsExempted = range (3)

# -----------
# -- Cnote --
# -----------

# Some precise coverage note, either expected or reported:

class Cnote:
    def __init__(self, kind):
        self.kind = kind
        self.segment = None

# -----------
# -- Xnote --
# -----------

# Expected note, as instanciated by an expectation pattern over a real
# source line:

class Xnote (Cnote):
    def __init__(self, xnp, block):
        Cnote.__init__ (self, xnp.kind)
        self.weak = xnp.weak
        self.block = block

        self.stext = xnp.stext
        self.nmatches = 0

        self.discharger = None  # The Enote that discharged this

    def register_match(self, segment):
        self.segment = segment
        self.nmatches += 1

# ------------
# -- XnoteP --
# ------------

# Expectation pattern, materializing the user level representation of a note
# expectation in drivers (text like "l+"). These get instanciated into a set
# of actual expected indications for precise segments when the line regular
# expressions are matched.

# Different kind of expectations instanciate differently on a given source
# line. We introduce specialized note factories for this purpose:

# Block notes are relevant for a general section. Eventhough the block is
# matched line by line, we need to materialize a single note for the whole
# block.

class XnoteP_block:

    def __init__(self, notep):
        self.notep  = notep
        self.lastni = None    # The last note instance we returned

    def instanciate_over(self, tline, block):

        # We create a single instance the first time around, then expand the
        # section over subsequence matches.

        if self.lastni:
            thisni = None
            self.lastni.segment.sloc1.l = tline.lno

        else:
            thisni = Xnote (xnp=self.notep, block=block)
            thisni.register_match (Section(
                    l0 = tline.lno, c0 = 0, l1 = tline.lno, c1 = 0))

        if thisni: self.lastni = thisni
        return thisni

# !block notes without a specific segment text are relevant to entire lines

class XnoteP_line:

    def __init__(self, notep):
        self.notep = notep

    def instanciate_over(self, tline, block):

        thisni = Xnote (xnp=self.notep, block=block)
        thisni.register_match (Line(tline.lno))

        return thisni

# !block notes with a specific segment subtext are relevant to that segment:
# we'll expect a reported note to designate a point within that subtext (most
# often, the beginning of it)

class XnoteP_segment:

    def __init__(self, notep, stext):
        self.notep = notep
        self.stext = stext

    def instanciate_over(self, tline, block):

        thisni = Xnote (xnp=self.notep, block=block)

        # Register matches for Segments corresponding to all the instances
        # of the subtext we find, then error out if too few or too many.

        [thisni.register_match (Segment (tline.lno, m.start()+1, m.end()))
         for m in re.finditer (self.stext, tline.text)]

        thistest.stop_if (
            thisni.nmatches == 0, FatalError (
                "couldn't find subtext '%s' in line '%s'"
                % (self.stext, tline.text)))

        thistest.stop_if (
            thisni.nmatches > 1, FatalError (
                "multiple matches of subtext '%s' in line '%s'"
                % (self.stext, tline.text)))

        return thisni

class XnoteP:

    NK_for = {'l0': lNoCode, 'l-': lNoCov, 'l!': lPartCov, 'l+': lFullCov,
              'l#': lx0, 'l*': lx1,
              's-': sNoCov, 's!': sPartCov,
              'dT-': dtNoCov, 'dF-': dfNoCov, 'd!': dPartCov, 'd-':dNoCov,
              'c!': cPartCov,
              'x0': xBlock0, 'x+': xBlock1,
              '0': None}

    def __init__(self, text, stext=None):
        self.weak = text[0] == '~'
        if self.weak: text = text[1:]

        self.kind = self.NK_for[text]
        self.stext = stext

        # We could require and use stext to store expected justification text
        # for exemptions. We don't handle that as of today.

        thistest.stop_if (
            False and self.stext == None and self.kind in xNoteKinds,
            FatalError ("expected justification text required for %s" % text))

        # Setup our instanciation factory now, which lets us perform the
        # required test only once:

        if block_p (self.kind):
            self.factory = XnoteP_block (notep=self)
        elif not self.stext:
            self.factory = XnoteP_line (notep=self)
        else:
            self.factory = XnoteP_segment (notep=self, stext=stext)

    def instanciate_over (self, tline, block):
        return self.factory.instanciate_over (tline, block)

# -----------
# -- Enote --
# -----------

# Emitted note, as extracted from an xcov report:

class Enote(Cnote):
    def __init__(self, kind, segment):
        self.kind = kind        # The kind of emitted note
        self.segment = segment  # The line segment it designates

        self.discharges = None  # The Xnote it discharges

# ---------------------------
# -- Dictionary facilities --
# ---------------------------

# Dictionary of lists with default on read:

class ListDict(dict):
    def __init__(self):
        dict.__init__(self)

    def __getitem__(self, key):
        if not dict.has_key(self, key):
            dict.__setitem__(self, key, [])
        return dict.__getitem__(self, key)

# Dictionary of coverage notes indexed by note kind:

class KnoteDict(dict):
    def __init__(self, possible_keys):
        [self.__setitem__(key, []) for key in possible_keys]

    def register(self, note):
        self[note.kind].append (note)

# ==========================
# == Text lines and files ==
# ==========================

class Tline:
    """Associate a line contents with its position in a text file."""
    def __init__(self, lno, text):
        self.lno = lno
        self.text = text

class Tfile:
    """Abstract a set of Tlines from a provided filename."""

    def __init__(self, filename, process):
        self.nlines = 0
        self.process = process
        self.tlines = [self.new_tline (text)
                       for text in open (filename)]

    def new_tline(self, text):
        self.nlines += 1
        tline = Tline(self.nlines, text)
        self.process (tline)
        return tline

    def contents(self):
        return self.tlines

# ====================
# == Note Expanders ==
# ====================

# Expose per-unit dictionaries of expected or reported coverage notes
# extracted from text files (=xcov or =report outputs, driver sources).
# These are used by SCOV_Helper to facilitate checks of emitted results
# vs expectations.
#
# The result dictionary keys are source names and values are KnoteDict
# objects (per kind dictionary of note instances)
#
# { [sourcename] -> { [note kind] -> [ Cnote, Cnote, ... ],
#                     ...
#                   },
#   ...
# }
#
# Below is a rough sketch of the entities and classes involved:
#
#   test_blob1.adb       blob1.adb
#   ...                      v
#   Expect.Patterns   >  XnotesExpander
#     (XnoteP)               v
#                 .xlnotes = { sourcename - KnoteDict(lNoteKinds) of Xnote }
#                 .xrnotes = { sourcename - KnoteDict(rNoteKinds) of Xnote }
#
# xcov --annotate=xcov   -> *.xcov
#                            v LnotesExpander
#                  .elnotes = { sourcename - KnoteDict(lNoteKinds) of Enote }
#
# xcov --annotate=report -> test.rep
#                            v RnotesExpander
#                  .ernotes = { sourcename - KnoteDict(rNoteKinds) of Enote }

# --------------------
# -- LnotesExpander --
# --------------------

# Construct a { source -> KnoteDict } dictionary of emitted Line Notes
# from =xcov outputs in files corresponding to a provided DOTXCOV_PATTERN
# (*.xcov for example).

class LnotesExpander:

    NK_for = {'.': lNoCode, '+': lFullCov, '-': lNoCov, '!': lPartCov,
              '#': lx0, '*': lx1}

    def process_tline(self, tline):
        m = re.match('\s*([0-9]+) (.):', tline.text)
        if m: self.elnotes[self.source].register (
            Enote (kind = self.NK_for[m.group(2)],
                   segment = Line(int(m.group(1)))))

    def listing_to_enotes(self, dotxcov):
        self.source = dotxcov.rsplit ('.', 1)[0]
        self.elnotes[self.source] = KnoteDict(lNoteKinds)
        Tfile (filename=dotxcov, process=self.process_tline)

    def __init__(self, dotxcov_pattern):

        # xcov --annotate=xcov produces a set of .xcov annotated unit sources,
        # each featuring a synthetic note per line.

        self.elnotes = {}
        [self.listing_to_enotes (dotxcov) for dotxcov in ls (dotxcov_pattern)]

# --------------------
# -- RnotesExpander --
# --------------------

# Construct a { source -> KnoteDict } dictionary of emitted Line Notes
# from =xcov outputs in files corresponding to a provided DOTXCOV_PATTERN

class RnotesExpander:
    """Produce list of Enote instances found in a "report" output."""

    NK_for = {"decision outcome FALSE never": dfNoCov,
              "decision outcome TRUE never": dtNoCov,
              "decision never evaluated": dNoCov,
              "decision not exercised in both directions": dPartCov,
              "multiple statement SCOs": sPartCov,
              "condition has no independent influence pair": cPartCov,
              "statement not executed": sNoCov}

    def nkind_for(self, ntext):

        # Search for a possible straight correspondance first

        for key in self.NK_for:
            if ntext.find (key) != -1:
                return self.NK_for [key]

        # If we haven't found a match, check for exemption notes. The text
        # will tell us if we have one, and we need to look at the number of
        # exempted violations to finalize.

        r = re.search (": (\d+) exempted violation", ntext)
        if r: return xBlock0 if int(r.group(1)) == 0 else xBlock1

        return None

    def to_enotes(self, report):

        # We need to ignore everything not in the report sections
        # of interest, so until we know we're in ...
        self.section = rsNoInterest

        self.report = report
        Tfile (filename=self.report, process=self.process_tline)

    def register(self, source, enote):
        if source not in self.ernotes:
            self.ernotes[source] = KnoteDict(rNoteKinds)
        self.ernotes[source].register (enote)

    def process_tline(self, tline):

        rline = tline.text

        # Figure out which section we're [getting] in.  Beware that
        # the ordering of the regexp checks matters here.

        if re.search ("NON-EXEMPTED VIOLATIONS", rline):
            self.section = rsNotExempted
            return None
        elif re.search ("EXEMPTED VIOLATIONS", rline):
            self.section = rsExempted
            return None
        elif re.match (".* (violation|region)\.$", rline):
            # Getting out of a section of interest ...
            self.section = rsNoInterest

        if self.section == rsNoInterest: return None

        # In section of interest, match the emitted note text. We expect
        # something like "andthen.adb:10:33: statement not covered",
        #                 -----------:-----: ---------------------
        #                 source name:segmt: note text (-> note kind)

        # First, try to figure out the source name, one of our main dictionary
        # keys. If no match at all, skip.

        xsrc = '([^ ]*)\.(ads|adb):(.*)'
        p = re.match(xsrc, rline)
        if not p:
            return None

        source = "%s.%s" % (p.group(1), p.group(2))

        # Then, work over the trailing part. Not stricly necessary, but
        # shorter so slightly more efficient.

        tail = p.group(3)

        nkind = self.nkind_for (tail)
        if nkind == None:
            thistest.failed (
                "(%s) '%s' ?" % (self.report, rline.rstrip('\n')))
            return None

        section = Section_within (tail)

        thistest.stop_if (
            not section,
            FatalError ("Unable to parse report line\n'%s'" % rline))

        return self.register (
            source, Enote (kind = nkind, segment = section))

    def __init__(self, report):

        # xcov --annotate=report produces a single report featuring a list of
        # indications for slocs in all the units.

        self.ernotes = {}
        self.to_enotes (report)


# --------------------
# -- XnotesExpander --
# --------------------

# Construct two { source -> KnoteDict } dictionaries of expected coverage
# notes, one for line notes and one for report notes as expressed by user
# expectations found in a provided XFILE.

# We refer to the expressed user expectations as SCOV data, and parse it
# according to the following grammar:

#     scov_data := ucx_list
#     ucx_list := ucx <newline> [ucx_list]
#     ucx := sources <new_line> lx_list
#     sources := "--# " filename_list
#     filename_list := FILENAME [filename_list]
#     lx_list := lx <newline> [lx_list]
#     lx := "-- " lx_lre lx_lnote_list [lx_rnote_list] <newline>
#     lx_lre := "/" REGEXP "/"
#     weak_mark := ~
#     cov_level_test := <s|d|m|u> "=>"
#     lx_lnote_list := lx_lnote_choice [";" lx_lnote_list]
#     lx_lnote_choice := [cov_level_test] [weak_mark] lx_lnote
#     lx_lnote := <l-|l!|l+|l*|l#|l0>
#     lx_rnote_list := lx_rnote_choice [lx_rnote_list]
#     lx_rnote_choice := [cov_level_test] [weak_mark] lx_rnote
#     lx_rnote := <s-|s!|dT-|dF-|d!|u!|m!|x0|x+>[:"TEXT"]

# The start of the SCOV data is identified as the first comment whose syntax
# matches a "sources" line.  Any comment before then is assumed to be a normal
# free-text comment.  Any comment thereafter is assumed to be part of the
# SCOV data.

# The "lx_lre" is a regular expression that is used to identify source lines
# that match "-- # " + lx_lre.  We decided to make the "-- # " implicit in
# order to reduce the lx_lre expression to a minimum as well as to force a
# standard format for all such markers.

# slashes inside lx_lre tokens are allowed. The SCOV_data parser simply
# uses the first and last slash as the delimiters.

# We use two intermediate abstractions to build the dictionaries from
# the expectations text:
#
# * Line Coverage eXpectations (LineCX) objects, to represent individual
#   expectations line like  "--  /bla/ l- s-", and
#
# * Unit Coverage eXpecations (UniCX) objects to represent the associations
#   of a sequence of line expectations with unit names.

# -----------
# -- Block --
# -----------

class Block:
    def __init__(self, parent):
        self.parent = parent

# ------------
# -- LineCX --
# ------------

class LineCX:
    """Line Coverage eXpectations class.  Associated with source file names,
    these define unit coverage expectation specs."""

    def __init__(self, lre, lnp, rnps):
        self.lre = lre
        self.lnp = lnp
        self.rnps = rnps

    def instanciate_lnotes_over(self, tline, block):
        return [self.lnp.instanciate_over (tline, block)]

    def instanciate_rnotes_over(self, tline, block):
        return [rnp.instanciate_over (tline, block)
                for rnp in self.rnps if rnp.kind]

# ------------
# -- UnitCX --
# ------------

class UnitCX:
    """Associate a source name with a list of expected Coverage Line
    eXpectations. Construct Line and Report Xnote dictionaries."""

    def locate_source(self, source):
        """Helper for __init__. Return valid relative path were SOURCE may be
        found, searching plausible locations from the instantiation point."""

        for pdir in ["src/", "../src/"]:
            if os.path.exists(pdir+source):
                return pdir+source

        raise FatalError ("Unable to locate source %s" % source)

    # expected notes instanciations
    # -----------------------------

    def instanciate_notes_for(self, lx, tline, block):
        [self.xldict.register (ln)
         for ln in lx.instanciate_lnotes_over (tline, block)]
        [self.xrdict.register (rn)
         for rn in lx.instanciate_rnotes_over (tline, block) if rn]

    # fuzz block processing
    # ---------------------

    # We identify block with the help of explicit comments, not with lone
    # language constructs such as begin/end in Ada. Finding the proper couples
    # of the latter is not easy and error prone.

    def blopen_p (self, tline):
        return re.match ("^\s*begin\s*-- #", tline.text)

    def blclose_p (self, tline):
        return re.match ("^\s*end;\s*-- #", tline.text)

    def check_block_on (self, tline):

        if self.blopen_p(tline):
            self.current_block = Block (parent = self.current_block)

        if self.blclose_p(tline):
            thistest.stop_if (
                not self.current_block,
                FatalError ("end of nonexistant block at\n=> " + tline.text))
            self.current_block = self.current_block.parent

    # toplevel processing
    # -------------------

    def process_tline(self, tline):
        [self.instanciate_notes_for (lx, tline, self.current_block)
         for lx in self.LXset if re.search (lx.lre, tline.text)]
        self.check_block_on (tline)

    def __init__(self, source, LXset):
        self.LXset = LXset

        # dictionaries of expected line and report notes for our unit

        self.xldict = KnoteDict(lNoteKinds)
        self.xrdict = KnoteDict(rNoteKinds)

        self.source = source
        self.current_block = None
        self.tfile  = Tfile (filename=self.locate_source(source),
                             process=self.process_tline)

        thistest.stop_if (
            self.current_block, FatalError ("fuzz block still open at EOF"))

# --------------------
# -- XnotesExpander --
# --------------------

class XnotesExpander:

    def __init__(self, xfile, xcov_level):
        self.xcov_level = xcov_level
        self.xlnotes = {}
        self.xrnotes = {}
        [self.to_xnotes(ux) for ux in
         self.__parse_scovdata (self.__get_scovdata (xfile))]

    def to_xnotes(self, ux):
        self.xlnotes [ux.source] = ux.xldict
        self.xrnotes [ux.source] = ux.xrdict

    def __get_scovdata(self, scov_file):
        """Return a list of strings containing the SCOV_data.
        To simplify parsing, the leading "--" is also stripped.
        """

        # The scov data begins at the first line that starts with
        # a '--#' comment.  Any line that starts as a comment after
        # this first '--#' line is assumed to be part of the scov data.
        # Build a list of lines containing the scov data stored in
        # scov_file now.
        contents = []
        in_scovdata = False
        for line in lines_of(scov_file):
            # Take care of leading/trailing spaces to give the user
            # more flexibility.  Also take care of the trailing new-line
            # character that we get from lines_of.
            line.strip()
            if line.startswith('--#'):
                in_scovdata = True
            if in_scovdata and line.startswith('--'):
                # Also take this opportunity to strip the leading '--'
                # as well as any space immediately followint it.  This
                # will simplify the parsing a little bit.
                contents.append(line[2:].lstrip())
        return contents

    def __parse_scovdata(self, scovdata):
        """Parse the given SCOV_DATA and return the corresponding
        list of UnitCX instances."""

        # The current UnitCX object being built.  We start a new UnitCX
        # everytime we see a "sources" line (which starts with '--#').
        current_ucx = (None, [])

        # The UXset being built while reading the scov data.
        UXset = []
        for line in scovdata:
            if line.startswith('#'):
                # We have finished reading the data for the current_ucx.
                # Build the associated UnitCX object, followed by resetting
                # current_ucx before we start reading the next ucx.
                if current_ucx[0] is not None:
                    UXset += [UnitCX(source=source,
                                         LXset=current_ucx[1])
                                for source in current_ucx[0]]
                current_ucx = (self.__parse_sources(line), [])
            else:
               # This must be an LX line.
               current_ucx[1].append(self.__parse_lcx(line))
        if current_ucx[0] is not None:
            [UXset.append (UnitCX(source=source, LXset=current_ucx[1]))
             for source in current_ucx[0]]
        return UXset

    def __parse_sources(self, image):
        """Given IMAGE as a string that contains a "sources" line,
        parse that line and return a list of source filenames."""

        # It's just a space-separated list of source files, with a leading
        # '#' character, so all we have to do is return a split of that
        # string, without the first '#'.
        return image.split()[1:]

    def __parse_one_expected_rnote(self, image):
        image=image.strip()
        if ':' in image:
            (noteim, stextim) = image.split(':')
            stext=stextim.strip('"')
        else:
            (noteim, stext) = (image, None)

        return XnoteP (text=self.__select_rnote(noteim), stext=stext)

    def __parse_expected_rnotes(self, image):
        if '#' in image:
            imlist = image.split('#')
        elif ',' in image:
            imlist = image.split(',')
        else:
            imlist = [image]
        return [self.__parse_one_expected_rnote(im) for im in imlist]

    def __parse_lcx(self, image):
        """Parse IMAGE as a string that contains a line expectation
        spec and return the corresponding LineCX object.
        """
        # Extract the LRE from the rest of the image.
        m = re.match("\s*/(.*)/\s+([^\s]*)( .*)?", image)
        if m is None:
            raise FatalError(
                "Invalid '%s' line expectation spec.\n" % image
                + "Expected /LRE/ lnote [rnotes]")

        lx_lre    = m.group(1)
        lx_lnote = XnoteP (text=self.__select_lnote (m.group(2)),
                           stext=None)

        thistest.stop_if (
            not m.group(3),
            FatalError ("Missing expected report notes in %s" % image))

        lx_rnotes = self.__parse_expected_rnotes(m.group(3))

        return LineCX("-- # (" + lx_lre + ")", lx_lnote, lx_rnotes)

    def __decode_note_choice(self, text):
        """Given a note_choice that depends potentially on the coverage
        level, return a list that represents this dependance,
        whose first element is the coverage level that it depends on
        (or an empty string if the note does not depend on the level)
        and whose second element is the expectation note if the choice
        is taken.

        For instance, given:

            'u => l!'

        ...this function will return:

            ['stmt+uc_mcdc', 'l!']

        """
        level_from_char = {"s" : "stmt",
                           "d" : "stmt+decision",
                           "m" : "stmt+mcdc",
                           "u" : "stmt+uc_mcdc"}
        sep = "=>"
        result = text.split(sep)

        if len(result) == 1:
            # No choice
            return ["", text]
        elif len(result) > 2:
            # Parse error
            raise FatalError("Note choice %s contains more than one arrow"
                             % text)
        else:
            return [level_from_char[result[0]], result[1]]

    def __select_lnote(self, text):
        """Decode text to return the line note for the current
        coverage level."""
        lx_lnote_list = text.split(";")
        level_table = dict([self.__decode_note_choice(conditional_note)
                            for conditional_note in lx_lnote_list])

        if not level_table.has_key(''):
            raise FatalError("No default case in line expectation: %s" % text)

        return fb_get(level_table, self.xcov_level)

    def __select_rnote(self, text):
        """Decode text to return the report note for the current
        coverage level."""
        level_table = dict([['', "0"], self.__decode_note_choice(text)])
        return fb_get(level_table, self.xcov_level)

# ======================================
# == SCOV_helper and internal helpers ==
# ======================================

# ---------------
# -- UXchecker --
# ---------------

class UXchecker:
    """Internal coverage marks checker class. This is used by SCOV_helper to
    compare sets of lines where coverage marks are expected with sets of lines
    where actual coverage marks were found in a report.

    A checker is instantiated to work over a given report for a specific
    source.  Its job will be to compare a provided set of expected marks
    against what is found in the report for specific kinds of marks."""

    # --------------
    # -- __init__ --
    # --------------
    def __init__(self, report, xdict, edict):
        self.n_failed_init = thistest.n_failed
        self.xdict = xdict
        self.edict = edict
        self.report = report

    def register_failure(self, comment):
        thistest.failed("("+self.report+") " + comment)

    def try_sat_over(self, ekind, xn):

        # See if expected note XN is satisfied by one of the emitted notes of
        # kind EKIND. Store to sat dictionary accordingly. Note that we only
        # check for section inclusions here, so don't validate the correctness
        # of exemption justifications at this stage.

        for en in self.edict [ekind]:
            if en.segment.within (xn.segment):
                en.discharges = xn
                xn.discharger = en
                self.sat[xn.block].append(xn)
                return

    def process_unsat(self, block):

        # Process unatisfied expected notes associated with BLOCK.
        # BLOCK is None for the set of expectations not part of a block.

        # Fuzzy matching: for deviations or positive notes, only raise failure
        # if none of the expected indications was satisfied (~OK if at least
        # one was emitted).

        # For each category, compute whether we have one expectation satisfied
        # in this fuzzy BLOCK:

        psat_p = False; dsat_p = False
        if block:
            for n in self.sat[block]:
                dsat_p |= deviation_p(n.kind)
                psat_p |= positive_p(n.kind)

        # Then complain about the unsatisfied stuff as necessary:

        for xn in self.unsat[block]:
            dev_p = deviation_p(xn.kind)
            pos_p = positive_p(xn.kind)
            if (not xn.weak
                and ((not dev_p and not pos_p)
                     or (dev_p and not dsat_p)
                     or (pos_p and not psat_p))):
                self.register_failure (
                    "Expected %s mark missing at %s"
                    % (NK_image[xn.kind], str(xn.segment)))

    def register_unsat(self, xn):
        self.unsat[xn.block].append(xn)

    def process_xkind (self, xkind, ekinds):

        # Process expected notes of kind XKIND looking for candidate
        # dischargers in emitted noted of kinds EKINDS.

        xnotes = self.xdict[xkind]

        self.sat = ListDict()
        [self.try_sat_over(ekind, xn)
         for xn in xnotes for ekind in ekinds if not xn.discharger]

        self.unsat = ListDict()
        [self.register_unsat(xn) for xn in xnotes if not xn.discharger]

        [self.process_unsat(block) for block in self.unsat]

    def process_ekind(self, ekind):

        # Process emitted notes of kind EKIND, after we're done processing
        # all the relevant expected notes.

        if not strict_p(ekind): return

        enotes = self.edict[ekind]

        [self.register_failure(
                "Unexpected %s mark at %s" %
                (NK_image[en.kind], str(en.segment)))
         for en in enotes if not en.discharges]

    def process_notes(self, relevant_note_kinds, discharge_kdict):
        thistest.log ("\n~~ processing " + self.report + " ~~\n")

        # For each kind in RELEVANT_NOTE_KINDS, process discharges of
        # expectations from emitted notes. DISCHARGE_KDICT provides a special
        # set of of emitted note kinds that may discharge a given kind of
        # expected note, when that set is wider than the target kind alone.

        # We have to do this with two distinct loops because there is no
        # guaranteed one to one correspondance between emitted vs expected
        # kinds for discharges.

        # Process expected notes first, complaining about those that are not
        # discharged (expected bla missing).

        [self.process_xkind(
                xkind = xkind, ekinds = discharge_kdict.get (xkind, [xkind]))
         for xkind in relevant_note_kinds]

        # Then process emitted notes, complaining about those that don't
        # discharge any expectation as required (unexpected blo).

        [self.process_ekind(ekind) for ekind in relevant_note_kinds]

        # Dump the report contents in case this check exposed a test failure:

        if  thistest.n_failed > self.n_failed_init:
            thistest.log("\nreport contents:\n")
            thistest.log(contents_of(self.report))
        else:
            thistest.log("OK\n")

# -----------------
# -- SCOV_helper --
# -----------------

class SCOV_helper:
    """Helper class for source coverage activities."""

    # --------------
    # -- __init__ --
    # --------------
    def __init__(self, drivers, xfile, category, xcovlevel):
        self.drivers = [os.path.basename(os.path.splitext(d)[0])
                        for d in drivers]
        self.category = category
        self.xcovlevel = xcovlevel

        # Internal attributes: Directory where the instantiation takes place,
        # base prefix of Working Directory names, and original expectations
        # file.

        self.homedir = os.getcwd()+"/"
        self.wdbase  = "tmp_"
        self.xfile   = xfile

        # { sourcename -> KnoteDict } dictionaries of emitted/expected
        # line/report notes. We'll extract emitted notes from reports when we
        # know they have been produced. We extract expected notes from the
        # provided expectation file

        self.elnotes = {}
        self.ernotes = {}

        xnotes = XnotesExpander (xfile, xcovlevel)
        self.xlnotes = xnotes.xlnotes
        self.xrnotes = xnotes.xrnotes

    # -----------------
    # -- consolidate --
    # -----------------
    def consolidate(self):
        """Whether SELF instantiates a consolidation test."""
        return len(self.drivers) > 1

    # ----------------
    # -- singletest --
    # ----------------
    def singletest(self):
        """Whether SELF instantiates a single test."""
        return len(self.drivers) == 1

    # ----------------
    # -- locate_ali --
    # ----------------
    def locate_ali(self,source):
        """Return the fullpath of the ali file corresponding to the given
        SOURCE file.  Return None if none was found.
        """

        # Whatever the kind of test we are (single or consolidation), we
        # expect every ALI file of interest to be associated with at least
        # one single test, and to be present in the "obj" subdirectory of
        # the associated working dir.

        # Compute the local path from single test workdir and iterate over
        # working dir for all our drivers until we find. There might actually
        # be several instances in the consolidation case. We assume they are
        # all identical, and they should be for typical situations where the
        # same sources were exercised by multiple drivers:

        lali="obj/"+os.path.basename(os.path.splitext(source)[0] + ".ali")
        for main in self.drivers:
            tloc=self.awdir_for(main)+lali
            if os.path.exists(tloc):
                return tloc

        return None

    # --------------
    # -- ali_list --
    # --------------
    def ali_list(self):
        """Return a list of ali files corresponding to the list of sources
        specified in this tests's UXset.
        """
        all_alis = {}
        for ali in [self.locate_ali(source) for source in self.xrnotes]:
            if ali:
                all_alis[ali] = 1
        return all_alis.keys()

    # ---------
    # -- run --
    # ---------
    def run(self, extracargs=""):
        """Evaluate source coverage as exercised by self.drivers over
        the sources in self.UXset. Compare actual errors with expected
        specs in the associated LXset."""

        self.log()

        # Whatever the kind of test, we get to a Working Directory and
        # switch back when done:
        self.to_workdir(self.rwdir())

        # For single tests (no consolidation), we first need to build and
        # xcov run to get an execution trace:
        if self.singletest():
            self.build(self.drivers[0],extracargs)
            self.alis = list_to_file(self.ali_list(), "alis.list")
            self.xcov_run(self.drivers[0])
        else:
            self.alis = list_to_file(self.ali_list(), "alis.list")

        # Then, whatever the kind of test again, run xcov to get actual
        # coverage reports and check against our Xpectation specs.
        self.gen_xcov_reports()
        self.check_expectations()

        self.to_homedir()
        thistest.flush()

    # -------------------------
    # -- working directories --
    # -------------------------

    # Single tests (no consolidation) are each assigned a particular
    # working directory, named after their main unit with a prefix.

    # Consolidation tests are assigned a separate working directory as
    # well, and need access to each of the single directories to retrieve
    # execution traces and binaries.

    def rwdir_for(self,main):
        """Relative path to Working Directory for single MAIN."""
        return self.wdbase + main + "/"

    def awdir_for(self,main):
        """Absolute path to Working Directory for single MAIN."""
        return self.homedir + self.rwdir_for(main)

    def rwdir(self):
        """Relative path to Working Directory for current instance."""

        # For a single test, discriminate with driver basename. For a
        # consolidation test, discriminate with the expectation file basename.
        # We need the latter to allow multiple consolidation scenarii for a
        # testcase.

        if self.singletest():
            return self.rwdir_for(self.drivers[0])
        else:
            return self.rwdir_for(
                os.path.basename(os.path.splitext(self.xfile)[0]))

    def awdir(self):
        """Absolute path to Working Directory for current instance."""
        return self.homedir+self.rwdir()

    # -----------
    # -- build --
    # -----------
    def build(self,main,extracargs):
        """gprBuild binary for main program MAIN"""

        # Seek a few tentative source dirs, for typical locations of test
        # sources from a working directory, and typical locations of common
        # support sources.

        gprbuild(
            gprfor ([main+".adb"], "gen",
                    ["../"*n + "src" for n in range (1, 6)]
                    + ["../"*n for n in range (1, 7)]),
            cargs = ["-g", "-gnateS", "-fpreserve-control-flow", "-gnatd.X",
                     "-gnata", "-gnat05"] + to_list(extracargs))

    # --------------
    # -- xcov_run --
    # --------------
    def xcov_run(self,main):
        """run MAIN through "xcov run" to produce an execution trace."""

        # Feed xcov run with full path (absolute dir) of the program so we
        # can directly get to the binary from the trace when reading it from
        # a different directory, such as in consolidation tests.

        ofile="xcov_run_%s.out" % main
        xrun([self.awdir_for(main)+main,
              "--level=%s" % self.xcovlevel, "--scos=@%s" % self.alis],
             out=ofile)

        thistest.fail_if (
            match ("!!! EXCEPTION RAISED !!!", ofile),
            "exception raised while running '%s'." % main);

    # -------------------------
    # -- gen_one_xcov_report --
    # -------------------------
    def gen_one_xcov_report(self, traces, format, options=""):
        """Helper for gen_xcov_reports, to produce one specific report for a
        particulat FORMAT, from a provided list of TRACES over a provided list
        of ALIS. The command output is saved in a file named FORMAT.out."""

        p = xcov (args = ['coverage', '--scos=@'+self.alis,
                          '--level='+self.xcovlevel,
                          '--annotate='+format, "@"+traces] + to_list(options),
                  out = format+".out")

    # ----------------------
    # -- gen_xcov_reports --
    # ----------------------

    def force_xcov_report(self, filename):

        if not os.path.exists (filename):
            report = open(filename, 'w')
            report.write ("dummy xcov report")
            report.close

    def gen_xcov_reports(self):
        """Generate the reports against which we will check expectation
        specs. Request both the xcov format, which produces .ad?.xcov outputs,
        and the report format, which gets saved as report.out."""

        traces = list_to_file(
            [self.awdir_for(main)+main+'.trace' for main in self.drivers],
            "traces.list")

        self.gen_one_xcov_report(traces, format="xcov")

        # When nothing of possible interest shows up for a unit, xcov
        # generates nothing at all. Create dummy reports here to prevent
        # fatal exceptions trying to open them downstream.
        [self.force_xcov_report(source+'.xcov') for source in self.xrnotes]

        self.gen_one_xcov_report(
            traces, format="report", options="-o test.rep")

    # ------------------------
    # -- check_expectations --
    # ------------------------
    def check_expectations(self):

        # Expand the reports into source->emitted-notes dictionaries
        # and check against our per-source expectations.

        self.elnotes = LnotesExpander("*.xcov").elnotes
        self.ernotes = RnotesExpander("test.rep").ernotes

        [self.check_expectations_over (source) for source in self.xrnotes]

    def check_expectations_over(self, source):
        """Process expectations for a particular source, comparing
        expected coverage marks against what is found in the xcov reports
        for this source."""

        frame ("Processing UX for %s" % (source), post=0, char='~').display()

        # When we're running for a level stricter than the test category
        # (e.g. running a stmt test with --level=stmt+decision), we
        #
        # * Just ignore some emitted notes, simply irrelevant for the catgory
        #   (e.g. dT-, which doesn't change the statement coverage status of
        #   the outer statement). This is conveyed by the rp_?notes_for sets.
        #
        # * Accept that some emitted notes discharge expectations of some
        #   other kind as well. This is conveyed by the discharge_kdict values
        #   below.

        stricter_level = strength [self.xcovlevel] > strength [self.category]

        # Line notes checks

        discharge_kdict = {
            # In stricter_level mode, we let ...

            # an emitted l! discharge an expected l+, when the l! is most
            # likely caused by irrelevant violations for the category
            lFullCov: [lFullCov, lPartCov],

            # an emitted lx1 discharge an lx0 expectation, when the extra
            # exempted violations are most likely caused by the level extra
            # strictness, hence irrelevant for the category
            lx0:      [lx0, lx1] } if stricter_level else {}

        UXchecker (
            source+'.xcov',
            self.xlnotes.get(source),
            self.elnotes.get(source, KnoteDict(lNoteKinds))
            ).process_notes(rp_lnotes_for[self.category], discharge_kdict)


        # Report notes checks

        discharge_kdict = {
            # In stricter_level mode, we let ...

            # an emitted xBlock1 discharge an xBlock0 expectation, as the
            # extra exempted violations are most likely irrelevant for the
            # category
            xBlock0: [xBlock0, xBlock1] } if stricter_level else {}

        UXchecker (
            'test.rep',
            self.xrnotes.get(source),
            self.ernotes.get(source, KnoteDict(rNoteKinds))
            ).process_notes(rp_rnotes_for[self.category], discharge_kdict)

    # ---------
    # -- log --
    # ---------
    def log(self):
        frame ("%s, %s\n%s coverage with --level=%s"
               % (str(self.drivers), self.xfile,
                  self.category, self.xcovlevel),
               char='*').display()

    # ----------------
    # -- to_workdir --
    # ----------------
    def to_workdir(self,wdir):
        """Switch to work directory WDIR, creating it if necessary. WDIR is
        expected to be either absolute or relative from the homedir."""

        self.to_homedir()
        mkdir(wdir)
        cd(wdir)

    # ----------------
    # -- to_homedir --
    # ----------------
    def to_homedir(self):
        """Switch to this test's homedir."""
        cd(self.homedir)

# ***************************************************************************
#   SOURCE COVERAGE SUPPORT LEVEL 2 : Search and run applicable drivers
# ***************************************************************************

# =================
# == ExerciseAll ==
# =================

class ExerciseAll:

    def expand_drivers(self, patterns):
        """Add to the list of drivers to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""

        for pattern in to_list(patterns):
            self.all_drivers += ls(pattern)

    def expand_shared_drivers(self):
        """Search and expand possible shared drivers uptree for our local
        functional units."""

        # shared drivers would be <updir>/test_<xx>*.adb for some possible
        # updir and every <xx> such that there is a src/<xx>*.adb.

        # gather *set* of <xx> candidates first, then expand the associated
        # possible lists of drivers (each maybe empty). Beware not to include
        # child or sub units, as these dont mirror as such in the set of test
        # drivers.

        sxx = set(srcmatch.group(1)
                  for srcmatch in (re.match ("([a-z_]*).*\.adb",
                                             os.path.basename(src))
                                   for src in ls ("src/*"))
                  if srcmatch)

        [self.expand_drivers (
                "%ssrc/test_%s*.adb" % (prefix, body))
         for prefix in ("../" * n for n in range(1, 3))
         for body in sxx]

    def __category(self):
        """Compute our test category from its directory location."""
        global TEST_DIR
        root_expr = "(Ada|SanityCheck)"
        if re.search (root_expr + ".stmt", TEST_DIR):
            return "stmt"
        elif re.search (root_expr + ".decision", TEST_DIR):
            return "decision"
        elif re.search (root_expr + ".mcdc", TEST_DIR):
            return "mcdc"
        else:
            raise FatalError(
                "Unable to determine test category from test dir: %s"
                %TEST_DIR)

    def drivers_from(self, cspec):
        """Compute the set of drivers that need to be combined for
        consolidation purposes, extracted from the consolidation spec in
        CSPEC."""

        # Extract the drivers regular expression to match from the
        # consolidation spec file, ...

        drv_expr = re.match ("drivers=(.*)", contents_of (cspec)).group(1)

        # ... then construct and return the retricted list of drivers that
        # match this expression

        return [drv for drv in self.all_drivers if re.search (drv_expr, drv)]

    def optrequest_in(self, options):
        """Whether the OPTIONS string of compilation options features
        a request for optimization."""
        return options and re.search ("(^|\s)-O", options)

    def __init__(self, extradrivers="", extracargs=""):

        # Step 1: Compute the list of drivers to exercise ...
        # ---------------------------------------------------

        # Probe all those from src/ plus those explicitely provided. If that
        # is empty, see if we have bodies aimed at being exercised by common
        # drivers up-tree. Abort if there's nothing to exercise at all

        self.all_drivers = []
        self.expand_drivers("src/test_*.adb " + extradrivers)

        if len(self.all_drivers) == 0:
            self.expand_shared_drivers()

        thistest.stop_if (
            len(self.all_drivers) == 0,
            FatalError ("Request to exercise empty test_set"))


        # Step 2: Determine a few test parameters common to all drivers
        # -------------------------------------------------------------

        # - test category:

        category = self.__category()

        # - Set of xcovlevel values to exercise:

        # If we have a qualification test and a common context level, use
        # that. Fallback to defaults otherwise.

        if qualification_test_p() and thistest.options.qualif_xcov_level:
            xcovlevels = [thistest.options.qualif_xcov_level]
        else:
            xcovlevels = default_xcovlevels_for [category]

        # - compilation arguments:

        # Account for provided compilation flags for qualif tests, then
        # append test specific extra compilation flags.

        testcargs = []
        if qualification_test_p():
            testcargs = to_list(thistest.options.qualif_cargs)

        testcargs += to_list (extracargs)

        # Step 3: Run the tests ...
        # -------------------------

        # First, run the test for each driver, individually.
        # for covlevel in xcovlevels:
        [[SCOV_helper(drivers=[driver], xfile=driver, category=category,
                     xcovlevel=covlevel).run(testcargs)
         for driver in self.all_drivers]
         for covlevel in xcovlevels]

        # Next, run applicable consolidation tests.
        consolidation_specs = ls ("src/cons_*.txt")
        for covlevel in xcovlevels:
            [SCOV_helper(drivers=self.drivers_from(cspec), xfile=cspec,
                         category=category, xcovlevel=covlevel).run(testcargs)
             for cspec in consolidation_specs]
