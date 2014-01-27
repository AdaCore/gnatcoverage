#!/usr/bin/env python

# ***************************************************************************
# ***                  COUVERTURE TESTSUITE MAIN DRIVER                   ***
# ***************************************************************************

"""./testsuite.py [OPTIONS] [RE_TEST_PATH]

Run the GNATcoverage testsuite

See ./testsuite.py -h for more help
"""

# ***************************************************************************

from gnatpython.env import Env
from gnatpython.ex import Run
from gnatpython.fileutils import mkdir, rm, ln, which, cp
from gnatpython.main import Main
from gnatpython.mainloop import (MainLoop, add_mainloop_options,
                                 SKIP_EXECUTION)
from gnatpython.optfileparser import OptFileParse
from gnatpython.reports import ReportDiff

from glob import glob

import traceback
import time
import logging, os, re, sys
import optparse

from SUITE import cutils
from SUITE.cutils import contents_of, FatalError, exit_if
from SUITE.cutils import version, list_to_tmp

from SUITE.dutils import pdump_to, pload_from
from SUITE.dutils import jdump_to, jload_from
from SUITE.dutils import time_string_from, host_string_from

from SUITE.qdata import stdf_in, qdaf_in, treeref_at
from SUITE.qdata import QLANGUAGES, QROOTDIR
from SUITE.qdata import QSTRBOX_DIR, CTXDATA_FILE
from SUITE.qdata import SUITE_context, TC_status, TOOL_info, OPT_info_from

from SUITE import control
from SUITE.control import BUILDER, XCOV
from SUITE.control import altrun_opt_for, altrun_attr_for
from SUITE.control import cargs_opt_for, cargs_attr_for
from SUITE.vtree import Dir, DirTree

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
#
#   * A level-specific set of GNAT configuration pragmas applies to all
#     compilations.
#
#   * Tests only are only run with --annotate=report, not --annotate=xcov,
#     as only the former is claimed to be qualified.

# A dictionary of information of interest for each qualification level:

class QlevelInfo:
    def __init__(self, levelid, subtrees, xcovlevel, pragmas):
        self.levelid   = levelid   # string identifier

        # regexp of directory subtrees: testdirs that match this
        # hold qualification tests for this level
        self.subtrees  = subtrees

        # --level argument to pass to xcov when running such tests when in
        # qualification mode
        self.xcovlevel = xcovlevel

        # Configuration pragmas (e.g. restrictions) to apply at this level
        self.pragmas = pragmas

RE_QCOMMON="(Common|Appendix)"
RE_QLANG="(%s)" % '|'.join (QLANGUAGES)

# A regular expression that matches subdirs of qualification tests
# that should apply for coverage criteria RE_CRIT.

def RE_SUBTREE (re_crit):
    return "%(root)s/((%(common)s)|(%(lang)s/(%(crit)s)))" % {
        "root": QROOTDIR, "common": RE_QCOMMON,
        "lang": RE_QLANG, "crit": re_crit
        }

# Note that we expect test directory names to be in unix form here.
# This is easy to achieve, will have obvious observable effects if not
# respected, and simplifies the regexp overall.

QLEVEL_INFO = {

    "doA" : QlevelInfo (
        levelid   = "doA",
        subtrees  = RE_SUBTREE (re_crit="stmt|decision|mcdc"),
        xcovlevel = "stmt+mcdc",

        # At level A, the need to perform MCDC and to process decisions in
        # expression relies on the use of short-circuit operators only:
        pragmas   = [
            "pragma Restrictions (No_Direct_Boolean_Operators);"]
        ),

    "doB" : QlevelInfo (
        levelid   = "doB",
        subtrees  = RE_SUBTREE (re_crit="stmt|decision"),
        xcovlevel = "stmt+decision",
        pragmas   = []
        ),

    "doC" : QlevelInfo (
        levelid   = "doC",
        subtrees  = RE_SUBTREE (re_crit="stmt"),
        xcovlevel = "stmt",
        pragmas   = []
        )
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
#        % Switches (main)   += "-fno-inline" as needed
#        % Switches (<lang>) += SUITE.control.BUILDER.COMMON_CARGS
#             |                 (-g -fdump-scos ...)
#             |
#             |   direct calls to gprbuild() from test.py,
#             |   or via TestCase(extracargs)
#             |       |
#             v       v           testsuite.py
#  gprbuild (gpr, extracargs)     [--cargs=<>] [--cargs:Ada=<>] [--cargs:C=<>]
#                     |               |
#                     o----> ADD <----o
#                             |
#                             v
#     run "gprbuild -Pgpr --cargs=... [-cargs:Ada=<>] [-cargs:C=<>]

# In addition to the SUITE.control bits, the only default option we enforce is
# -gnat05 for Ada.

# =============================
# == Setup specific controls ==
# =============================

# This toplevel driver supports various command line options allowing
# customization of the test execution process:
#
# * --gnatcov_<cmd> allows providing alternate programs to execute instead of
#   "gnatcov <cmd>" when a test normally needs it.  A typical use is
#
#        --gnatcov_run=</path/to/program_execution_driver>
#
#   to provide a replacement to "gnatcov run" for execution trace production,
#   e.g. running the program on hardware through a probe.
#
#   The replacement programs may be python scripts or native executables for
#   the host on which the testsuite runs. They need to support the command
#   line interface of the facility they replace, at least the subset used by
#   the tests.
#
#   When the alternate command is called, the current directory is that of
#   the context at the point of the call. We don't switch to the directory
#   which holds the alternate implementation prio to calling it.
# 
#   The control.ALTRUN_GNATCOV_PAIRS variable contains the list of ('gnatcov',
#   <cmd>) pairs we support.  See the altrun/example subdir for implementation
#   examples.
#
# * --pre/post-testsuite/testcase allows providing programs to execute as
#   hooks within the testsuite process:
#
#   - Before the complete test sequence starts (--pre-testsuite)
#   - After the complete test sequence has finished (--post-testsuite)
#   - Before each testcase executes (--pre-testcase)
#
#   A typical use is with environments requiring on-board execution through a
#   probe, which might need some service to startup before any program loading
#   may take place, some shutdown operation afterwards (once done with all the
#   tests), and possibly some preliminary local cleanup before each test can
#   start.
#
#   When pre-testcase is called, the current directory is set to the testcase
#   location. When pre/post-testsuite is called the current directory is set
#   to the location where the hook hook script or binary resides.
#
#   The control.ALTRUN_HOOK_PAIRS variable contains the list of
#   ('pre|post', 'testsuite|testcase') pairs we support.

# For environments that need combinations of the aforedescribed facilities,
# the --altrun=<path> command line option provides a convenient way to
# wrap everything together.
#
# It instructs this driver to look in the <path> subdirectory as part
# of the testsuite run preparation, and then:
#
# * If there is a "setup" binary (exe or .py) there, run it with the
#   current directory set to <path>; then:
#
# * If there are binaries (exe or .py) matching the pre/post command option
#   names, use each as if the corresponding option had been passed
#   
# * Likewise if there are binaries corresponding to the gnatcov_<cmd> option
#   names, except a "c<cmd>" binary is searched to match each "gnatcov_<cmd>"
#   option.

# ===============
# == TestSuite ==
# ===============

class TestSuite:

    # --------------------------
    # -- GAIA file facilities --
    # --------------------------

    def __init_logdir (self):

        self.log_dir = os.path.join (self.root_dir, 'output')
        mkdir(self.log_dir)

        [open(os.path.join(self.log_dir, f), 'w').close()
         for f in ('comment', 'results', 'discs', 'altrun')]

    def __logpath (self, filename):
        return os.path.join(self.log_dir, filename)

    def __push_log (self, textlist, filename):
        """Append the list of lines in TEXTLIST to the GAIA log FILENAME."""

        # If there's nothing to push, return. Empty lists can show up here,
        # e.g. an empty dead list for a run with filter or stopped by
        # consecutive failures. We must be careful not to dump a possibly
        # invalid empty line in the output file in such a case.

        if not textlist:
            return

        with open(self.__logpath(filename), mode='a') as fd:
            fd.write ('\n'.join (textlist) + '\n')

    def __push_comments (self, textlist):
        self.__push_log (
            textlist = textlist, filename = 'comment')

    def __push_results (self, textlist):
        self.__push_log (
            textlist = textlist, filename = 'results')

    def __push_altrun (self, textlist):
        self.__push_log (
            textlist = textlist, filename = 'altrun')

    def __discriminants_log(self):
        return self.__logpath ('discs')

    # -------------------------------
    # -- STR production facilities --
    # -------------------------------

    def __init_strbox (self):
        """Initialize the directory where the STR production artifacts
        will be dropped."""
        self.strbox_dir = os.path.join (self.root_dir, QSTRBOX_DIR)
        mkdir(self.strbox_dir)

    def __dump_ctxdata(self):
        """Dump the testsuite context data file for use by the STR report
        producers."""

        tprefix = self.__target_prefix()
            
        if self.options.other_tool_info:
            (toolname, version_info) = Run(
                [sys.executable, self.options.other_tool_info], timeout=20
                ).out.split('##')
            
            other_tool_info = TOOL_info (
                exename=toolname, ver=version_info)
        else:
            other_tool_info = None

        jdump_to (
            CTXDATA_FILE,
            SUITE_context(
                runstamp = time_string_from(time.localtime()),
                host     = host_string_from(self.env.host),
                treeref  = treeref_at("."),
                cmdline  = " ".join(sys.argv),

                options  = OPT_info_from(options=self.options),

                gnatpro  = TOOL_info (tprefix+"gcc"),
                gnatemu  = TOOL_info (tprefix+"gnatemu"),
                gnatcov  = TOOL_info ("gnatcov"),
                other    = other_tool_info
                )
            )


    # -----------------------
    # -- Common facilities --
    # -----------------------

    def __target_prefix (self):
        return self.env.target.triplet+'-' if self.options.target else ""

    def __check_consistency_with_previous_runs(self, ctxdata_file):
        """Check consistency between this run and the previous ones.

        The purpose of this check is to verify that this testsuite run
        is executed using the very same parameters, thus allowing us
        to reuse the results from previous runs.

        RETURN VALUE
            A list of error messages, each message being a string
            documenting an inconsistency.  Return None otherwise.
        """
        if not os.path.isfile(ctxdata_file):
            return ('  * Missing testsuite data file (%s)' % ctxdata_file,)

        ref_ctx = jload_from(ctxdata_file)
        tprefix = self.__target_prefix()
        gnatpro = TOOL_info(tprefix+"gcc")
        gnatemu = TOOL_info(tprefix+"gnatemu")
        gnatcov = TOOL_info("gnatcov")

        errors = []
        for check in (('host platform',
                       self.env.host.platform, ref_ctx.host.platform),
                      ('target platform',
                       self.env.target.platform, ref_ctx.target.platform),
                      ('gnatpro version',
                       gnatpro.version, ref_ctx.gnatpro.version),
                      ('gnatemu version',
                       gnatemu.version, ref_ctx.gnatemu.version),
                      ('gnatcov version',
                       gnatcov.version, ref_ctx.gnatcov.version),
                      ('--cargs option',
                       self.options.cargs, ref_ctx.options.cargs),
                     ):
            (msg, new, expected) = check
            if new != expected:
                errors.append('  * %s mismatch: "%s" (expected "%s")'
                              % (msg, new, expected))

        for lang in control.KNOWN_LANGUAGES:
            new = getattr(self.options, cargs_attr_for(lang))
            expected = getattr(ref_ctx.options, cargs_attr_for(lang))
            if new != expected:
                errors.append('  * --cargs:%s option mismatch:'
                              ' "%s" (expected "%s")'
                              % (lang, new, expected))

        if not errors:
            # Return None instead of the empty list (the only reason
            # for doing so is that it's a little more pythonic).
            errors = None
        return errors

    def __reuse_testcase_previous_run(self, test):
        """Return True iff we should be reusing the the previous run's results.

        This includes verifying that the testcase has in fact been
        previously run, and that the various associated reports are
        still available for re-use (see TestCase.has_previously_run).

        PARAMETERS:
            test: a TestCase object.
        """
        if not self.env.main_options.skip_if_ok:
            # The user has not asked us to re-use any previous result...
            return False

        if not test.has_previously_run():
            return False

        # Make sure that the testing environment has not changed
        # since this previous run.  Otherwise, it's not safe to re-use
        # those results.
        errors = self.__check_consistency_with_previous_runs(test.ctxf())
        if errors is not None:
            # Log of the reasons why the testcase results could not
            # be reused. This may help future investigations.
            logging.debug("Cannot re-use the previous run's results:")
            for e in errors:
                logging.debug(e)
            return False

        tcs = test.latched_status()
        return tcs.status in ('OK', 'UOK')

    # ------------------------
    # -- Object constructor --
    # ------------------------

    def __init__(self):
        """Prepare the testsuite run: parse options, compute and dump
        discriminants, compute lists of dead/non-dead tests, run gprconfig and
        build the support library for the whole series of tests to come"""

        # Latch our environment values, then setup the log directory and
        # initialize the GAIA log files. We need to do that before setting
        # up the altrun hooks, as their execution may dump output logs.

        self.env = Env()
        self.root_dir = os.getcwd()

        self.__init_logdir ()

        # Parse command lines options, also setting self.enable_valgrind to
        # convey whether tests should be run under valgrind control:

        self.options = self.__parse_options()
        self.enable_valgrind = (
            None if self.options.bootstrap_scos else
            self.options.enable_valgrind)

        # Setup profile hooks, if any, then resolve path argument values as
        # needed while we have visibility on where relative paths lead:

        self.__setup_altrun_hooks()
        self.__resolve_paths()

        # Add current directory in PYTHONPATH, allowing TestCases to find the
        # SUITE and SCOV packages:

        self.env.add_search_path('PYTHONPATH', self.root_dir)

        # If trying to update a previous run by only re-running
        # the testcases that failed in that previous run, make sure
        # that this new run is consistent with the previous ones.
        if self.options.skip_if_ok:
            errors = self.__check_consistency_with_previous_runs(CTXDATA_FILE)
            if errors:
                raise FatalError(
                    'Cannot use --skip-if-ok'
                    ' (incompatible testing environment)\n'
                    'The following errors were detected:\n'
                    + '\n'.join(errors))

        # Perform the environment adjustments required to run the compilation
        # toolchain properly:

        self.setup_toolchain (self.options.toolchain)

        # Setup trace directories for bootstrap runs:

        if self.options.bootstrap_scos != None:
            self.trace_dir = os.path.join (self.log_dir, 'traces')
            rm(self.trace_dir, recursive=True)
            mkdir(self.trace_dir)
        else:
            self.trace_dir = None

        # Generate the discs list for test.opt parsing, and dump the list in
        # a file that we can then to determine which discriminants were set
        # during a particular run:

        self.discriminants = self.__discriminants()
        self.__push_log (
            textlist = [" ".join(self.discriminants)],
            filename = self.__discriminants_log()
            )

        # Setup the STR box and dump the suite data file, for qualification
        # runs. Note that we must be in a revision controlled tree in this
        # case, so we can fetch a local reference for consistency comparisons.
        #
        # Dump the path to our python interpreter as well in this case, so the
        # STR generation shell script can find and reuse it. Use a simplified
        # format and location for this purpose (text, at toplevel).

        if self.options.qualif_level:
            self.__init_strbox()
            self.__dump_ctxdata()

            with open ('python_bin.dump', 'w') as f:
                f.write ('%s' % sys.executable)

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

        # Setup configuration files for Restrictions control

        self.__setup_control_pragmas()

        # Initialize counter of consecutive failures, to stop the run
        # when it is visibly useless to keep going

        self.n_consecutive_failures = 0

    # ---------------------
    # -- control pragmas --
    # ---------------------

    def __setup_control_pragmas (self):
        """Dump the configuration files that tests expect to
           find depending on our execution mode."""

        # At this stage, tests expect a gnat.<dolevel> file for any
        # qualification run.

        dolevel = self.options.qualif_level

        if not dolevel:
            return

        f = open ("gnat.%s" % dolevel, 'w')
        f.write (
            '\n'.join (
                ["--  pragmas for a gnatcov %s qualification run" % dolevel]
                + QLEVEL_INFO[dolevel].pragmas) + '\n'
            )
        f.close()

    # -----------------------------------
    # -- Early comments about this run --
    # -----------------------------------

    def __options_comment (self):
        return "Testsuite options:\n" + " ".join(_quoted_argv())

    def __versions_comment (self):

        prefix = self.__target_prefix()
        all_versions = [
            version ("gnatcov") + ", " + version (prefix+"gnatls")
            ]

        if self.env.main_options.target:
            all_versions.append (version (prefix+"gnatemu", nlines=2))

        return '\n'.join (
            ["Running versions:"] + all_versions) + '\n'

    def __early_comments (self):
        return [
            '\n\n'.join (
                [self.__options_comment (),
                 self.__versions_comment ()])
            ]

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
            + self.__toolchain_discriminants()
            )


    def __base_discriminants(self):
        return ['ALL'] + self.env.discriminants

    def __cargs_discriminants(self):
        """Compute a list of discriminants (string) for each switch passed in
        all the --cargs command-line option(s).  The format of each
        discriminant CARGS_<X> where <X> is the switch stripped of its
        leading dashes.

        For instance, if this testsuite is called with --cargs='-O1'
        --cargs:Ada='-gnatp', then this function should return
        ['CARGS_gnatp', 'CARGS_O1'].

        Return an empty list if --cargs was not used.
        """

        allopts = ' '.join (
            [getattr (self.env.main_options, attr)
             for attr in (
                    cargs_attr_for(l)
                    for l in [None] + control.KNOWN_LANGUAGES)]
            )
        return ["CARGS_%s" % arg.lstrip('-') for arg in allopts.split()]

    def __qualif_level_discriminants(self):
        """List of single discriminant (string) denoting our current
        qualification mode, if any. This is ['XXX'] when invoked
        with --qualif-level=XXX, [] otherwise"""

        return (
            [] if not self.env.main_options.qualif_level
            else [self.env.main_options.qualif_level.upper()]
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

        # ex --RTS=native or --RTS=kernel, or no --RTS at all

        else:
            return ["RTS_FULL"]

    def __toolchain_discriminants (self):
        """Compute the list of discriminants that reflects the version of the
        particular toolchain in use, if any. There's at most a single value in
        this list at this stage, e.g. ["7.0.2"] for /path/to/gnatpro-7.0.2.
        The match is on the sequence of three single digits separated by dots,
        possibly followed by "rc", then by maybe a '/' prior to the end of
        string."""

        m = re.search ("(\d\.[01]\.[0123](?:rc)?)/?$", self.options.toolchain)
        return [m.group(1)] if m else []

    def __generate_group (self, dirname, group_py):
        """Helper for the "test.py" research: generate a tree of testcases for
        the given "group.py" located in "dirname". Return whethet generation
        was successful."""

        group_py_path = os.path.join(dirname, group_py)
        p = Run(
            [sys.executable, group_py_path],
            timeout=20)

        if p.status != 0:
            raise FatalError(
                '\n'.join (
                    ['Test instance generation failed (%s)' % group_py_path,
                     p.out])
                )

    # ---------------------
    # -- __next_testcase --
    # ---------------------

    def __next_testcase_from (self, root):
        """Helper generator function for __next_testcase, producing a sequence
        of testcases to be executed from a provided root directory, updating
        self.run_list and self.dead_list on the fly. The testcase path ids are
        canonicalized into unix form here."""

        if not self.options.quiet:
            logging.info(
                "Searching for tests, %s ...",
                ("matching '%s' from %s" % (
                        self.tc_filter if self.tc_filter else "unfiltered",
                        root))
                )

        test_py = "test.py"
        group_py = "group.py"

        # Build a Directory Tree Object abstraction for this walk, which we
        # will use to maintain properties regarding the path leading to each
        # node.

        this_dto = DirTree ()

        # If there are multiple subdirs to traverse before reaching the root
        # starting point of our search, we need to check if any of them holds
        # a group generation request.

        # We will start at the first intermediate subdir then walk straight
        # through each component before searching tests for real. We maintain
        # a stack-list of remaining intermediate subdirs for this purpose,
        # assuming the root we're provided with is a unix style kind of path.

        # For a root like          ...      we first construct:
        #
        #   a/b/c  (relative)               idirs = ['a', 'b', 'c']
        #   /x/y/z (absolute)               idirs = ['/x', 'y', 'z']
        #
        # Then reverse so we can pop() entries in sequence:

        idirs = root.strip('/').split('/')
        if root[0] == '/':
            idirs[0] = '/' + idirs[0]

        idirs = [idir for idir in reversed (idirs)]

        for (dirname, subdirs, files) in os.walk(
            top = idirs.pop(), topdown = True, followlinks=True
            ):

            # Unixify the directory name early and make sure that we have at
            # least a trailing '/' to match expectations in our filtering
            # patterns. Trailing slashes in filters are useful to disambiguate
            # multiple subdirs with a common prefix at the same level, for
            # example to focus on "Qualif/C/" vs "Qualif/Common".

            dirname = dirname.replace ('\\', '/') + '/'

            # Build the Directory object abstraction for this subdir, map
            # it into our Directory Tree object and update our path-related
            # attributes of interest:

            diro = this_dto.topdown_map (dirname, subdirs, files)

            # For each node, we maintain a list of 'extra.opt' files
            # available uptree, useful to implement shared test control
            # for entire subtrees:

            diro.extraopt_uptree = (
                [] if diro.pdo is None
                else (
                    diro.pdo.extraopt_uptree + [
                        os.path.join(diro.pdo.fspath, 'extra.opt')]
                    ) if 'extra.opt' in diro.pdo.files
                else \
                    diro.pdo.extraopt_uptree
                )

            # If there is some testcases generation to do in this dir, first do
            # it, and then continue to look for tests.
            # TODO: look for a way to remove generated files that failed tests
            # do not rely on.

            if group_py in files:
                self.__generate_group (dirname, group_py)

            # Walk straight to the next intermediate dir entry, if any.

            if idirs:
                subdirs[:] = [idirs.pop()]
                continue

            # If there is not test to execute in this dir or the dir name
            # doesn't match the filter current filter, continue with the next
            # candidate subdir:

            if (test_py not in files
                or not re.search (
                    pattern=self.tc_filter, string=dirname)
                ):
                continue

            # Otherwise, instantiate a Testcase object and proceed:

            tc = TestCase (
                diro = diro,
                filename  = dirname + test_py,
                trace_dir = self.trace_dir
                )
            tc.parseopt(suite_discriminants = self.discriminants)

            if tc.killcmd:
                self.dead_list.append(tc)
            else:
                self.run_list.append(tc)
                yield tc

    def __next_testcase (self):
        """Generator for MainLoop, producing a sequence of testcases to be
        executed, updating self.run_list and self.dead_list on the fly."""

        # If we have a testcase filter designating a directly accessible
        # directory, no point in searching elsewhere:

        if os.path.isdir(self.tc_filter):
            roots = (self.tc_filter, )
        else:
            roots = ("Qualif/", "tests/")

        return (
            tc for root in roots
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
        self.tally = {}

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
                self.options.mainloop_jobs
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
            logging.info (
              ", ".join ([ "%d %s" % (count, status)
                           for (status, count) in self.tally.items()]))

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

    # ------------------------------
    # -- run_testcase and helpers --
    # ------------------------------

    def __prepare_testcase(self, test, timeout):

        # Setup test execution related files. Clear them upfront to prevent
        # accumulation across executions and bogus reuse of old contents if
        # running the test raises a premature exception, before the execution
        # script gets a chance to initialize the file itself.

        outf = test.outf()
        logf = test.logf()
        diff = test.diff()
        qdaf = test.qdaf()

        [cutils.clear (f) for f in (outf, logf, diff, qdaf)]

        # Save a copy of the context data in case the user wants to
        # re-run the testsuite with --skip-if-* later on.  Since
        # this context data is only generated when in Qualification
        # mode, only make that copy when in that mode, too.
        if self.options.qualif_level:
            cp(CTXDATA_FILE, test.ctxf())

        # Construct the test command line

        testcase_cmd = [sys.executable,
                        os.path.join(test.rtestdir, test.filename),
                        '--report-file=' + outf,
                        '--log-file=' + logf,
                        '--target', self.env.target.platform,
                        '--timeout', str(timeout)]
        if self.enable_valgrind:
            testcase_cmd.append('--enable-valgrind=' + self.enable_valgrind)
        if self.trace_dir is not None:
            test_trace_dir = os.path.join(test.trace_dir, str(test.index))
            mkdir(test_trace_dir)
            testcase_cmd.append('--trace_dir=%s' % test_trace_dir)

        # Propagate our command line arguments as testcase options.
        #
        # Beware that we're not using 'is not None' on purpose, to prevent
        # propagating empty arguments.

        mopt = self.env.main_options

        # In qualification mode, pass the target qualification level to
        # qualification tests and enforce the proper xcov-level

        if mopt.qualif_level and test.qualif_levels ():
            testcase_cmd.append('--qualif-level=%s' % mopt.qualif_level)
            testcase_cmd.append(
                '--xcov-level=%s' % QLEVEL_INFO[mopt.qualif_level].xcovlevel)

        if mopt.board:
            testcase_cmd.append('--board=%s' % mopt.board)

        if mopt.gprmode:
            testcase_cmd.append('--gprmode')

        if mopt.kernel:
            testcase_cmd.append('--kernel=%s' % mopt.kernel)

        if mopt.toolchain:
            testcase_cmd.append (
                '--toolchain=%s' % self.__toolchain_discriminants()[0]
                )

        if mopt.RTS:
            testcase_cmd.append('--RTS=%s' % mopt.RTS)

        testcase_cmd.append('--tags=@%s' % self.__discriminants_log())

        # --gnatcov_<cmd> family

        [testcase_cmd.append \
             ('--%(opt)s=%(val)s' % {
                    'opt' : altrun_opt_for(pgm, cmd),
                    'val' : getattr(mopt, altrun_attr_for(pgm, cmd))
                    }
              )
         for (pgm, cmd) in control.ALTRUN_GNATCOV_PAIRS
         if getattr(mopt, altrun_attr_for(pgm, cmd)) is not None]

        # --cargs family

        [testcase_cmd.append(
                '--%(opt)s=%(val)s' % {
                    "opt" : cargs_opt_for(lang),
                    "val" : getattr(mopt, cargs_attr_for(lang))
                    }
                )
         for lang in [None] + control.KNOWN_LANGUAGES]

        return testcase_cmd

    def run_testcase(self, test, _job_info):
        """MainLoop hook to run a single non-dead TEST instance. If limit is
        not set, run rlimit with DEFAULT_TIMEOUT"""

        logging.debug("Running " + test.diro.fspath)

        if self.__reuse_testcase_previous_run(test):
            logging.debug("(reusing the previous run's result)")
            test.start_time = time.time()
            return SKIP_EXECUTION

        timeout = test.getopt('limit', default=DEFAULT_TIMEOUT)

        self.maybe_exec (self.options.pre_testcase, edir=test.atestdir)

        testcase_cmd = self.__prepare_testcase (test=test, timeout=timeout)

        test.start_time = time.time()

        return Run(
            testcase_cmd, output=test.diff(), bg=True, timeout=int(timeout))

    # --------------------
    # -- collect_result --
    # --------------------

    def __log_results_for(self, test):
        """Internal helper for collect_result."""

        # Several things to do once a test has run:

        # 1) If the execution failed, arrange to get a link to the err log
        # where the infrastructure expects it (typically not in the test
        # dedicated subdirectory where the original log resides)

        if not test.passed:
            odiff = self.odiff_for(test)
            cutils.clear (odiff)
            ln(test.diff(), odiff)

        # 2) log and populate the "results" file for gaia: file the test
        # status + possible comment on failure

        self.__push_results ([
                ''.join (
                    ["%s:%s" % (test.rname(), test.status),
                     ":%s" % test.comment.strip('"') if (
                            not test.passed and test.comment)
                     else ""]
                    )
             ])

        # 3) Log the execution status as needed on stdout. All tests are
        # logged in !quiet mode.  Real failures are always logged.

        dsec = test.end_time - test.start_time

        if (not self.options.quiet) or (not test.passed and not test.xfail):
            logging.info (
                "%-68s %s - %s %s" % (
                    test.rtestdir,
                    "%02d m %02d s" % (dsec / 60, dsec % 60),
                    test.status, "(%s)" % test.comment if test.comment else "")
                )

        # Keep tally of executed tests by status

        self.tally[test.status] = \
          1 if test.status not in self.tally \
          else self.tally[test.status] + 1

        # Dump errlog on unexpected failure

        if self.options.diffs and not test.passed and not test.xfail:
            logging.info("Error log:\n" + contents_of (test.diff()))

    def __check_stop_after(self, test):
        """Internal helper for collect_result. Check if we need to stop the
        whole testsuite run after the execution of TEST."""

        if test.status == 'FAILED':
            self.n_consecutive_failures += 1
        else:
            self.n_consecutive_failures = 0

        if self.n_consecutive_failures >= 10:
            raise FatalError (
                "Stopped after %d consecutive failures"
                % self.n_consecutive_failures)

    def collect_result(self, test, _process, _job_info):
        """MainLoop hook to collect results for a non-dead TEST instance
        once it has run. Take the opportunity to perform post-run cleanups
        on request if the test succeded."""

        test.end_time = time.time()

        test.compute_status()

        if self.options.qualif_level:
            test.latch_status()

        self.__log_results_for(test)
        self.__check_stop_after(test)

        if test.status != 'FAILED' and self.options.do_post_run_cleanups:
            test.do_post_run_cleanups()
        
    def odiff_for(self, test):
        """Returns path to diff file in the suite output directory.  This file
        is used to generate report and results files."""

        return os.path.join(self.log_dir, test.rname() + '.out')

    # -------------------
    # -- parse_options --
    # -------------------

    def __parse_options(self):
        """Parse command lines options"""

        m = Main(add_targets_options=True)
        add_mainloop_options (m, extended_options=True)
        m.add_option('--quiet', dest='quiet', action='store_true',
                     default=False, help='Quiet mode. Display test failures only')
        m.add_option('--diffs', dest='diffs', action='store_true',
                     default=False, help='show diffs on stdout')
        m.add_option("--old-res", dest="old_res", type="string",
                        help="Old testsuite.res file")

        m.add_option('--post-run-cleanups', dest='do_post_run_cleanups',
                     action='store_true', default=False,
                     help='request post-run cleanup of temporary artifacts')

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

        m.add_option(
            '--other-tool-info', dest='other_tool_info',
            metavar='OTHER_TOOL_INFO', default="",
            help=(
                'Name of a python script outputing other '
                'tool##version info')
            )

        # --pre|post family

        [m.add_option(
                '--%s' % altrun_opt_for(when, what),
                dest=altrun_attr_for(when, what), default=None,
                help='run CMD %s to %s run' % (when, what), metavar="CMD")
         for (when, what) in control.ALTRUN_HOOK_PAIRS]

        m.add_option(
            '--altrun', dest="altrun", metavar="ALTRUN_SUBDIR",
            default=None,
            help='name of custom hooks directory')

        # shared options

        control.add_shared_options_to (m, toplevel=True)

        # Parse what options we do have on our command line, then perform a
        # couple of validity checks and compute bits of internal state for
        # later use:

        m.parse_args()

        # Determine the test filtering regexp

        m.options.run_test = m.args[0] if m.args else ""

        # Enforce a default -gnat<version> for Ada, so each test can expect an
        # explicit setting to filter on. Expect an explicit one if we're
        # running for qualification, making sure we know what target language
        # we're qualifying for.

        attr_cargs_ada = cargs_attr_for("Ada")
        cargs_ada = getattr(m.options, attr_cargs_ada)
        if not re.search ("-gnat95|-gnat05|-gnat12", cargs_ada):
            if m.options.qualif_level:
                raise FatalError (
                    "Missing -gnat<95|05|12> in cargs:Ada for qualification")
            else:
                setattr(m.options, attr_cargs_ada, cargs_ada + " -gnat05")

        return m.options

    # ---------------------
    # -- __resolve_paths --
    # ---------------------

    def __resolve_paths(self):
        """For options containing path values expressed possibly
        as relative from where the testsuite was launched, resolve
        to absolute paths."""

        attributes_to_resolve = (
            ["kernel", "altrun"]
            + [altrun_attr_for(p0, p1) for (p0, p1) in
               control.ALTRUN_HOOK_PAIRS + control.ALTRUN_GNATCOV_PAIRS]
            )

        [setattr(self.options, attr,
                 os.path.abspath (getattr(self.options, attr)))
         for attr in attributes_to_resolve
         if getattr(self.options, attr) is not None]

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

    # -----------------------------
    # -- altrun hooks & friends --
    # -----------------------------

    def maybe_exec (self, bin, edir=None):
        """Execute the provided BIN program file, if any. Keep the current
        directory untouched if EDIR is None. Otherwise, adjust the current
        working dir temporarily for this particular execution; to the location
        where BIN resides if EDIR is "...", or to EDIR's value otherwise."""

        if not bin:
            return
        
        to_run = [sys.executable, bin] if bin.endswith('.py') else [bin]

        cwd = os.getcwd()
        to_dir = (
            os.path.dirname(bin) if edir == "..."
            else edir if (edir is not None and edir != cwd)
            else None)
        try:
            if to_dir: os.chdir(to_dir)
            p = Run (to_run)
        finally:
            if to_dir: os.chdir(cwd)

        self.__push_altrun (["", "running hook: %s" % bin, p.out])

        exit_if (
            p.status != 0,
            "Altrun hook failed (%s):\n" % bin + p.out
            )

    def __bin_for(self, base, indir=None):
        """For a provided BASE filename (directory path allowed), return
        the name of the file can be used as an executable program on the
        current host. This includes python scripts as well as binary
        executables. If INDIR is not None, check if a binary for INDIR/BASE
        exists, but leave INDIR out of the return value.

        For a "path/to/pgm" base on Unix, this would be
        - "path/to/pgm.py" if this file exists, or
        - "path/to/pgm" on Unix, if this file exists, or
        - "path/to/pgm.exe" on Windows, if this file exists.

        Return None if no candidate executable exists. Issue a fatal error
        if there is more than one possibility.
        """

        candidate_exe = (
            base + (self.env.host.os.exeext if self.env.host.os.exeext else ""))
        candidates = [candidate_exe, base+".py"]

        def relative_for(p):
            return p if indir is None else os.path.join(indir, p)

        programs = [
            p for p in candidates if os.path.exists(relative_for(p))]

        exit_if (
            len (programs) > 1,
            "from %s, too many binaries for %s:\n%s" % \
                (os.getcwd(), base, '\n'.join(programs))
            )
        return programs[0] if programs else None

    def __setup_altrun_hooks (self):
        """Finalize the altrun/<subdir> for this run, if any, and install the
        pre/post hooks available from there."""

        if not self.options.altrun:
            return

        ctldir = self.options.altrun
        
        # Run the altrun subdir setup code, if any, then check each possible
        # hook of interest. Switch to the local directory for the setup step,
        # designed for local operations:

        self.maybe_exec (
            bin=self.__bin_for("setup", indir=ctldir), edir=ctldir)

        def install_altrun_for (p0, p1, binbase):
            """Establish an implicit value for the --P0_P1 command line option
            if we find a matching binary program in the altrun subdir we are
            processing. BINBASE provides the binary base name to use."""

            bin = self.__bin_for(os.path.join(ctldir, binbase))

            if not bin:
                return

            attr = altrun_attr_for(p0, p1)
            exit_if (
                getattr(self.options, attr),
                "%s altrun conflicts with explicit --%s" % (bin, attr)
                )

            self.__push_altrun (["hooking %s to %s" % (attr, bin)])
            setattr (self.options, attr, bin)

        # For the toplevel testsuite driver hooks, map on binaries
        # matching the command line option names:

        [install_altrun_for (
                p0=when, p1=what, binbase=altrun_opt_for(when, what))
         for (when, what) in control.ALTRUN_HOOK_PAIRS]

        # For the gnatcov <command> replacements, map on binaries called
        # 'c<command>':

        [install_altrun_for (p0=pgm, p1=cmd, binbase="c%s" % cmd)
         for (pgm, cmd) in control.ALTRUN_GNATCOV_PAIRS]
        
# ==============
# == TestCase ==
# ==============

class TestCase(object):

    # Index to assign to the next instance of this class
    index = 0

    def __init__(self, diro, filename, trace_dir=None):
        """Create a new TestCase for the given FILENAME within the DIRO
        directory object, both relative to the current directory. If TRACE_DIR
        is specified, save the bootstrap traces there."""

        self.diro         = diro

        self.rtestdir     = self.diro.fspath
        self.atestdir     = os.path.join(os.getcwd(), self.rtestdir)
        self.filename     = os.path.basename(filename)

        self.expected_out = None
        self.opt          = None
        self.trace_dir    = trace_dir

        self.index        = TestCase.index
        TestCase.index += 1

    def __lt__(self, right):
        """Use relative testdir alphabetical order"""
        return self.rtestdir < right.rtestdir

    # ---------------------------------
    # -- Testcase options and status --
    # ---------------------------------

    def __try_killcmd(self, cmd, opt):
        """See if the killing command CMD applies to this testcase according
        to the provided set of parsed OPTions. Set self.killcmd to the
        corresponding text for GAIA reports."""

        value = opt.get_value (cmd)
        self.killcmd = (
            None if value is None
            else "%s:%s" % (cmd, value)
            )

    def __trykill_from(self, opt):
        """See if the provided OPTions parsed for our discriminants
        trigger a DEAD or SKIP for this test. Set killcmd accordingly."""

        [self.__try_killcmd (cmd=cmd, opt=opt)
         for cmd in ('DEAD', 'SKIP') if not self.killcmd]


    def parseopt(self, suite_discriminants):
        """Parse the local test.opt + possible extra.opt uptree in
        accordance with the provided SUITE_DISCRIMINANTS + the test
        specific discriminants, if any."""

        tags = suite_discriminants + self.discriminants()

        test_opt = os.path.join(self.atestdir, 'test.opt')
        self.opt = (
            OptFileParse(tags, test_opt) if os.path.exists(test_opt)
            else None
            )
        self.expected_out = self.getopt('out', 'test.out')

        # Determine whether anything kills this test for the provided set
        # of discriminants. Check the local test.opt first, then possible
        # instances of extra.opt uptree:

        self.killcmd = None

        if self.opt:
            self.__trykill_from(self.opt)

        [self.__trykill_from (OptFileParse(tags, extraopt))
         for extraopt in self.diro.extraopt_uptree if not self.killcmd]

    def getopt(self, key, default=None):
        """Get the value extracted from test.opt that correspond to key
        If key is not found. Returns default.
        """
        return (
            self.opt.get_value(key, default_value=default) if self.opt
            else default
            )

    def compute_status(self):
        """Compute self.(passed, xfail, status, comment) where *passed* is
        the bare execution status, *xfail* tells whether there is an XFAIL
        moderator in test.opt for this test, *comment* is the associated
        test.opt comment, and *status* is the gaia OK/UOK/XFAIL/FAILED
        indication."""

        outf = self.outf()
        self.passed = (
            cutils.match("==== PASSED ==================", outf)
            if os.path.exists(outf) else False)

        # Compute the status of this test from the combination of its
        # execution success and a possible failure expectation. Retrieve the
        # relevant test.opt comment along tha way:

        xfail_comment = self.getopt('xfail', None)
        failed_comment = self.getopt('failed', None)

        self.comment = (
            xfail_comment if xfail_comment
            else failed_comment if failed_comment
            else "" )

        self.xfail = xfail_comment is not None
        status_dict = {
            # PASSED?   XFAIL => status    !XFAIL => status
              True:    {True:    'UOK',    False:    'OK'},
              False:   {True:    'XFAIL',  False:    'FAILED'}}

        self.status = status_dict[self.passed][self.xfail]

    def latch_status(self):
        pdump_to (
            self.stdf(),
            o = TC_status (
                passed=self.passed,
                xfail=self.xfail,
                status=self.status,
                comment=self.comment)
            )

    def latched_status(self):
        return pload_from (self.stdf())

    def do_post_run_cleanups(self):
        """Cleanup temporary artifacts from the testcase directory."""

        # In principle, most of this is the spawned test.py responsibilty,
        # because _it_ knows what it creates etc.  We have artifacts of our
        # own though (dump files for qualif runs, for example), and removing
        # these correctly can only be done from here. Doing the rest as well
        # is just simpler and more efficient.

        [rm (os.path.join(self.atestdir, gp), recursive=True)
         for gp in ('tmp_*', 'st_*', 'dc_*', 'mc_*', 'uc_*', 'obj', 'obj_*',
                    '[0-9]', '*.adb.*', 'test.py.log', '*.dump')
         ]

    # --------------------------------------
    # -- Testscase specific discriminants --
    # --------------------------------------

    def discriminants (self):
        """List of discriminants for this particular test. Might
        include LANG_<lang> if path to test contains /<lang>/ for any
        of the languages we know about."""

        discs = []

        lang = self.lang()
        if lang:
            discs.append ('LANG_%s' % lang.upper())

        return discs

    # ---------------------------
    # -- Testcase output files --
    # ---------------------------

    def outf(self):
        """Return the name of the file where outputs of the provided
        test object should go. Same location as the test source script,
        with same name + a .out extra suffix extension."""
        return os.path.join(self.atestdir, self.filename + '.out')

    def logf(self):
        """Similar to outfile, for the file where logs of the commands
        executed by the provided test object should go."""
        return os.path.join(self.atestdir, self.filename + '.log')

    def diff(self):
        """Similar to outf, for the file where diffs of the provided test
        object should go."""
        return os.path.join(self.atestdir, self.filename + '.err')

    def qdaf(self):
        return qdaf_in(self.atestdir)

    def stdf(self):
        return stdf_in(self.atestdir)

    def ctxf(self):
        """The file containing a SUITE_context describing the testcase run

        (the file is in pickle format).
        """
        return os.path.join(self.atestdir, 'ctx.dump')

    def has_previously_run(self):
        """Return True iff this testcase looks like it's been run before.

        REMARKS
            This function does not check the testcase status at all.
        """
        # At a minimum, a testcase produces the following files which
        # the testsuite uses to collect the results.
        return (os.path.isfile(self.stdf())
                and os.path.isfile(self.qdaf())
                and os.path.isfile(self.ctxf())
                and os.path.isfile(self.outf()))

    # -------------------------
    # -- Testcase properties --
    # -------------------------

    def rname(self):
        """A unique representative name for TEST, to be used in GAIA web URLs."""

        # Start from the path to test.py, remove the value-less parts like a
        # "./" prefix or a "/" suffix", and replace slashes which would
        # introduce problematic articial layers in URLs eventually. Note that
        # we expect the paths to have been unixified here.

        return self.rtestdir.strip('./').replace('/', '-')

    def qualif_levels(self):
        """List of qualification levels to which SELF applies"""

        # Check which QLEVEL subtrees would match ...
        return [
            qlevel for qlevel in QLEVEL_INFO
            if re.search (QLEVEL_INFO[qlevel].subtrees, self.rtestdir)]

    def lang(self):
        """The language specific subtree SELF pertains to"""
        for lang in control.KNOWN_LANGUAGES:
            if self.rtestdir.find ("/%s/" % lang) != -1:
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
    tso = TestSuite()
    tso.maybe_exec (bin=tso.options.pre_testsuite, edir="...")
    tso.run()
    tso.maybe_exec (bin=tso.options.post_testsuite, edir="...")


