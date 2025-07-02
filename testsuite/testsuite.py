#!/usr/bin/env python

"""./testsuite.py [OPTIONS] [RE_TEST_PATH]

Run the GNATcoverage testsuite

See ./testsuite.py -h for more help
"""

import logging
import time
import os
import re
import sys
import itertools

from e3.fs import mkdir, rm, cp
from e3.os.fs import which, unixpath
from e3.os.process import Run, quote_arg
import e3.testsuite
from e3.testsuite.control import AdaCoreLegacyTestControlCreator
from e3.testsuite.driver import TestDriver
from e3.testsuite.driver.classic import TestAbortWithError
from e3.testsuite.result import Log, TestResult, TestStatus
from e3.testsuite.testcase_finder import ParsedTest, TestFinder

import SUITE.cutils as cutils

from SUITE.cutils import strip_prefix, contents_of, lines_of
from SUITE.cutils import FatalError, exit_if
from SUITE.cutils import version

from SUITE.dutils import pdump_to, pload_from
from SUITE.dutils import jdump_to
from SUITE.dutils import time_string_from, host_string_from

from SUITE.qdata import stdf_in, qdaf_in, treeref_at
from SUITE.qdata import QLANGUAGES, QROOTDIR
from SUITE.qdata import QSTRBOX_DIR, CTXDATA_FILE
from SUITE.qdata import SUITE_context, TC_status, TOOL_info, OPT_info_from

import SUITE.control as control

from SUITE.control import BUILDER, _runtime_info
from SUITE.control import altrun_opt_for, altrun_attr_for
from SUITE.control import cargs_opt_for, cargs_attr_for

VALGRIND_TIMEOUT_FACTOR = 2
"""
When the testsuite runs with Valgrind (--enable-valgrind), the default timeout
is multiplied by this number to get the actual default timeout. This is used to
compensate the slowdown that Valgrind incurs.
"""

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
#   * Even if empty, an explicit set of GNAT configuration pragmas shall be
#     provided by way of a -gnatec= compilation switch, typically enforcing
#     what the Operational Conditions of Use mandate.
#
#   * Tests only are only run with --annotate=report, not --annotate=xcov,
#     as only the former is claimed to be qualified.

# A dictionary of information of interest for each qualification level:


class QlevelInfo(object):
    def __init__(self, levelid, subtrees, xcovlevel):
        self.levelid = levelid  # string identifier

        # regexp of directory subtrees: testdirs that match this
        # hold qualification tests for this level
        self.subtrees = subtrees

        # --level argument to pass to xcov when running such tests when in
        # qualification mode
        self.xcovlevel = xcovlevel

    def hosts(self, test_dir):
        """
        Whether the subtrees covered by this Qlevel encompass the
        provided test directory, which may be relative to a testsuite
        root or absolute.
        """

        # Expect filtering subtrees using forward slashes, so make
        # sure the provided test_dir is amenable so such patterns.
        return re.search(pattern=self.subtrees, string=unixpath(test_dir))


RE_QCOMMON = "(Common|Appendix)"
RE_QLANG = "(%s)" % "|".join(QLANGUAGES)

# A regular expression that matches subdirs of qualification tests
# that should apply for coverage criteria RE_CRIT.


def RE_SUBTREE(re_crit):
    return "%(root)s/((%(common)s)|(%(lang)s/(%(crit)s)))" % {
        "root": QROOTDIR,
        "common": RE_QCOMMON,
        "lang": RE_QLANG,
        "crit": re_crit,
    }


# Note that we expect test directory names to be in unix form here.
# This is easy to achieve, will have obvious observable effects if not
# respected, and simplifies the regexp overall.

QLEVEL_INFO = {
    "doA": QlevelInfo(
        levelid="doA",
        subtrees=RE_SUBTREE(re_crit="stmt|decision|mcdc"),
        xcovlevel="stmt+mcdc",
    ),
    "doB": QlevelInfo(
        levelid="doB",
        subtrees=RE_SUBTREE(re_crit="stmt|decision"),
        xcovlevel="stmt+decision",
    ),
    "doC": QlevelInfo(
        levelid="doC", subtrees=RE_SUBTREE(re_crit="stmt"), xcovlevel="stmt"
    ),
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

# On top of this user level control, we add a couple of flags such as -g or
# -fdump-scos automatically (depending on command line options, in particular
# on the requested kind of trace mode). Below is a sketch of the internal
# compilation flags flow:
#
#  SUITE.control.BUILDER.SCOV_CARGS(options) (-g -fdump-scos ...)
#   |
#   |      gprfor ()
#   |        template.gpr
#   |        % Switches (main)   += "-fno-inline" as needed
#   |            |
#   |            |   direct calls to gprbuild() from test.py,
#   |            |   or via TestCase(extracargs)
#   |            |       |
#   |            v       v         testsuite.py
#   |  gprbuild (gpr, extracargs)  [--cargs=<>] [--cargs:Ada=<>] [--cargs:C=<>]
#   |                    |              |
#   o--------------------o--------------o
#                        |
#                        v
#   run "gprbuild -Pgpr -cargs=... [-cargs:Ada=<>] [-cargs:C=<>]

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
#   - After each testcase executes (--post-testcase)
#
#   A typical use is with environments requiring on-board execution through a
#   probe, which might need some service to startup before any program loading
#   may take place, some shutdown operation afterwards (once done with all the
#   tests), and possibly some preliminary local cleanup before each test can
#   start or after each test has run.
#
#   When pre/post-testcase is called, the current directory is set to the
#   testcase location. When pre/post-testsuite is called the current directory
#   is set to the location where the hook script or binary resides.
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


def maybe_exec(log, binfile, args=None, edir=None):
    """
    Execute the provided BINFILE program file, if any.

    Run this program in the current working directory EDIR is None. Otherwise,
    run it in the location where BINFILE resides if EDIR is "...", or in EDIR's
    value otherwise.

    Pass the provided list of ARGS, if any, on the command line. Skip possible
    empty or None arg values there.

    Log the execution of this program in LOG, with possible output if the
    command fails.

    Return the process object.
    """

    if not binfile:
        return

    to_run = (
        [sys.executable, binfile] if binfile.endswith(".py") else [binfile]
    )

    if args:
        to_run.extend([arg for arg in args if arg])

    if edir == "...":
        edir = os.path.dirname(binfile)

    p = Run(to_run, cwd=edir)

    log += "\nRunning hook: {}\n".format(binfile)
    log += p.out

    return p


class TestPyRunner:
    """Helper to run a "test.py" test script."""

    filename = "test.py"

    def __init__(self, driver, result, test_prefix, working_prefix):
        """
        Common test driver initialization.

        :param driver: TestDriver instance that owns `self`.
        :param result: TestResult instance to fill in.
        :param test_prefix: Directory in the sources where this testcase was
            found. This is the directory that contains the "test.py" to run.
        :param working_prefix: Temporary directory to run this test.
        """
        self.driver = driver
        self.result = result
        self.test_prefix = test_prefix
        self.working_prefix = working_prefix

        # Convenience shortcut
        self.env = driver.env

        # Create an empty working directory. Even if testcases are ran in the
        # original tree, there are still some temporaries that we generate
        # elsewhere.
        os.mkdir(self.working_dir())

        # Create a "canonical" directory name for this testcase, useful to
        # simplify some platform-independent processings.
        self.unix_test_dir = unixpath(self.test_dir())

        # Load all relevant *.opt files to control the execution of this test
        self.test_control_creator = self.parse_opt()

    # Shortcuts to build paths, similar to
    # TestDriver.test_dir/TestDriver.working_dir.

    def test_dir(self, *args):
        return os.path.join(self.test_prefix, *args)

    def working_dir(self, *args):
        return os.path.join(self.working_prefix, *args)

    # ---------------------------
    # -- Testcase output files --
    # ---------------------------

    def outf(self):
        """
        Name of the file where outputs of the provided test object should go.

        Same location as the test source script, with same name + a .out extra
        suffix extension.
        """
        return self.test_dir(self.filename + ".out")

    def logf(self):
        """
        Similar to outfile, for the file where logs of the commands executed by
        the provided test object should go.
        """
        return self.test_dir(self.filename + ".log")

    def errf(self):
        """
        Similar to outf, for the file where diffs of the provided test object
        should go.
        """
        return self.test_dir(self.filename + ".err")

    def qdaf(self):
        return qdaf_in(self.test_dir())

    def ctxf(self):
        """
        The file containing a SUITE_context describing the testcase run (the
        file is in pickle format).
        """
        return self.test_dir("ctx.dump")

    # --------------------------------------
    # -- Testscase specific discriminants --
    # --------------------------------------

    def discriminants(self):
        """
        List of discriminants for this particular test. Might include
        LANG_<lang> if path to test contains /<lang>/ for any of the languages
        we know about.
        """
        discs = []

        lang = self.lang()
        if lang:
            discs.append("LANG_%s" % lang.upper())

        return discs

    def lang(self):
        """The language specific subtree SELF pertains to."""
        for lang in control.KNOWN_LANGUAGES:
            if "/{}/".format(lang) in self.unix_test_dir:
                return lang
        return None

    def lookup_extra_opt(self):
        """Look for all "extra.opt" files that apply to this testcase.

        The result is in bottom up order (deepest files in the tree first).
        """
        result = []

        # Determine the directory under which all tests reside. In an ideal
        # world, this would be the tessuite root directory, but in practice we
        # often run the testsuite on "Qualif/ ../extra/tests/", so if there is
        # an "extra" directory above the root directory, allow one level up.
        root_dir = self.driver.test_env["testsuite_root_dir"]
        up_root = os.path.abspath(os.path.join(root_dir, ".."))
        if os.path.exists(os.path.join(up_root, "extra")):
            root_dir = up_root

        # Climb up from the testcase directory to the testsuite root directory
        # and gather all extra.opt files found in the way.
        d = self.test_dir("..")
        while d.startswith(root_dir):
            extra_opt = os.path.join(d, "extra.opt")
            if os.path.exists(extra_opt):
                result.append(extra_opt)
            d = os.path.dirname(d)

        return result

    def parse_opt(self):
        """
        Parse the local test.opt + possible extra.opt uptree in accordance with
        the testsuite discriminants + the test specific discriminants, if any.
        The deeper .opt file prevails.
        """

        # Build a list of strings corresponding to the catenation of
        # test.opt/extra.opt files for this test, fetched bottom up in
        # directory tree order, then feed that to the opt file parser.
        opt_files = []

        test_opt = self.test_dir("test.opt")
        if os.path.exists(test_opt):
            opt_files.append(test_opt)

        opt_files.extend(self.lookup_extra_opt())

        opt_lines = sum((lines_of(f) for f in opt_files), [])

        # Create a single "control.opt" file to contain all control directives
        control_opt = self.working_dir("control.opt")
        with open(control_opt, "w") as f:
            for line in opt_lines:
                f.write(line + "\n")

        return AdaCoreLegacyTestControlCreator(
            system_tags=self.env.suite_discriminants + self.discriminants(),
            opt_filename=control_opt,
        )

    def set_up(self):
        mopt = self.env.main_options

        # Setup test execution related files. Clear them upfront to prevent
        # accumulation across executions and bogus reuse of old contents if
        # running the test raises a premature exception, before the execution
        # script gets a chance to initialize the file itself.

        outf = self.outf()
        logf = self.logf()
        errf = self.errf()
        qdaf = self.qdaf()

        for f in (outf, logf, errf, qdaf):
            cutils.clear(f)

        # Save a copy of the context data in case the user wants to
        # re-run the testsuite with --skip-if-* later on.  Since
        # this context data is only generated when in Qualification
        # mode, only make that copy when in that mode, too.
        if mopt.qualif_level:
            cp(CTXDATA_FILE, self.ctxf())

        # Construct the test command line

        testcase_cmd = [
            sys.executable,
            self.test_dir(self.filename),
            "--report-file=" + outf,
            "--log-file=" + logf,
        ]

        if mopt.enable_valgrind:
            testcase_cmd.append("--enable-valgrind=" + mopt.enable_valgrind)

        # Propagate our command line arguments as testcase options.
        #
        # Beware that we're not using 'is not None' on purpose, to prevent
        # propagating empty arguments.

        # In qualification mode, pass the target qualification level to
        # qualification tests and enforce the proper xcov-level. Note that
        # if we reach here, we already know that this test is relevant to
        # the requested level (validated by GNATcovTestFinder.probe):

        if mopt.qualif_level:
            testcase_cmd.append("--qualif-level=%s" % mopt.qualif_level)
            testcase_cmd.append(
                "--xcov-level=%s" % QLEVEL_INFO[mopt.qualif_level].xcovlevel
            )

        if mopt.build:
            testcase_cmd.append("--build=%s" % mopt.build)

        if mopt.target:
            testcase_cmd.append("--target=%s" % mopt.target)

        if mopt.board:
            testcase_cmd.append("--board=%s" % mopt.board)

        if mopt.gprmode:
            testcase_cmd.append("--gprmode")

        if mopt.trace_mode:
            testcase_cmd.append("--trace-mode=%s" % mopt.trace_mode)

        if mopt.kernel:
            testcase_cmd.append("--kernel=%s" % mopt.kernel)

        if mopt.trace_size_limit:
            testcase_cmd.append(
                "--trace-size-limit=%s" % mopt.trace_size_limit
            )

        if mopt.RTS:
            testcase_cmd.append("--RTS=%s" % mopt.RTS)

        if mopt.largs:
            testcase_cmd.append("--largs=%s" % mopt.largs.strip())

        testcase_cmd.append("--tags=@%s" % self.env.discr_file)

        if mopt.auto_arch:
            testcase_cmd.append("--auto-arch")

        if mopt.consolidate:
            testcase_cmd.append("--consolidate=%s" % mopt.consolidate)

        if mopt.pretty_print:
            testcase_cmd.append("--pretty-print")

        if mopt.spark_tests:
            testcase_cmd.append("--spark-tests=%s" % mopt.spark_tests)

        if mopt.all_warnings:
            testcase_cmd.append("--all-warnings")

        if mopt.default_dump_trigger:
            testcase_cmd.append(
                f"--default-dump-trigger={mopt.default_dump_trigger}"
            )

        if mopt.default_dump_channel:
            testcase_cmd.append(
                f"--default-dump-channel={mopt.default_dump_channel}"
            )

        if mopt.block:
            testcase_cmd.append("--block")

        if mopt.ada_version:
            testcase_cmd.append(f"--ada-version={mopt.ada_version}")

        # --gnatcov_<cmd> family

        for pgm, cmd in control.ALTRUN_GNATCOV_PAIRS:
            if getattr(mopt, altrun_attr_for(pgm, cmd)) is None:
                continue
            testcase_cmd.append(
                "--%(opt)s=%(val)s"
                % {
                    "opt": altrun_opt_for(pgm, cmd),
                    "val": getattr(mopt, altrun_attr_for(pgm, cmd)),
                }
            )

        # --gpr<tool> family
        for pgm in control.ALTRUN_GPR:
            if getattr(mopt, altrun_attr_for(pgm)) is None:
                continue
            testcase_cmd.append(
                f"--{altrun_opt_for(pgm)}="
                f"{getattr(mopt, altrun_attr_for(pgm))}"
            )

        # --cargs family

        for lang in [None] + control.KNOWN_LANGUAGES:
            testcase_cmd.append(
                "--%(opt)s=%(val)s"
                % {
                    "opt": cargs_opt_for(lang),
                    "val": getattr(mopt, cargs_attr_for(lang)),
                }
            )

        # Compute the testcase timeout, whose default vary depending on whether
        # we use Valgrind.
        timeout = int(self.test_control.opt_results["RLIMIT"])
        if mopt.enable_valgrind:
            timeout = VALGRIND_TIMEOUT_FACTOR * timeout

        if mopt.rewrite and not self.test_control.xfail:
            testcase_cmd.append("--rewrite")

        self.testcase_cmd = testcase_cmd
        self.testcase_timeout = timeout

    def maybe_exec(self, binfile, args=None, edir=None):
        """
        Shortcut for the global maybe_exec. Log the result in
        ``self.result.log`` and abort the testcase on failure.
        """
        if not binfile:
            return

        if maybe_exec(self.result.log, binfile, args, edir).status != 0:
            raise TestAbortWithError("Altrun hook failed ({})".format(binfile))

    def run(self):
        mopt = self.env.main_options

        self.maybe_exec(
            mopt.pre_testcase, args=[mopt.altrun], edir=self.test_dir()
        )

        # Run the "test.py" script in the testsuite root directory (as
        # expected: the script will change its own CWD later).
        start_time = time.time()
        self.test_py_process = Run(
            self.testcase_cmd,
            cwd=self.env.root_dir,
            timeout=self.testcase_timeout,
        )
        end_time = time.time()

        self.result.time = end_time - start_time

        # To ease debugging, copy the consolidated standard outputs (stdout +
        # stderr) to the "test.py.err" file.
        with open(self.errf(), "w") as f:
            f.write(self.test_py_process.out)

        # If the script exitted with an error status code, consider that the
        # testcase failed.
        self.test_py_failed = self.test_py_process.status != 0
        if self.test_py_failed:
            # Make --show-error-output display the consolidated standard
            # outputs, which likely contains relevant information for
            # debugging.
            self.result.log = Log(self.test_py_process.out)
        else:
            # Otherwise, load the test actual output from the "test.py.out"
            # file to the result, for --show-error-output.
            with open(self.outf()) as f:
                self.result.log = Log(f.read())

    def tear_down(self):
        args = self.env.main_options

        # Execute a post-testcase action if requested so, before the test
        # artifacts might be cleared by a post-run cleanup:

        self.maybe_exec(
            args.post_testcase, args=[args.altrun], edir=self.test_dir()
        )

        # Perform post-run cleanups if requested so. Note that this may
        # alter the test execution status to make sure that unexpected cleanup
        # failures get visibility:

        if self.result.status != TestStatus.FAIL and args.do_post_run_cleanups:
            self.do_post_run_cleanups(ts_options=args)

        if args.qualif_level:
            self.latch_status()

    def analyze(self):
        if self.test_py_failed:
            self.push_failure(
                "test.py exitted with status code {}".format(
                    self.test_py_process.status
                )
            )
        elif cutils.match("==== PASSED ==================", self.outf()):
            self.push_success()
        else:
            self.push_failure("Missing PASSED tag in output file")

    def run_test(self, previous_values, slot):
        """Run the testcase, analyze its result and push the result."""
        try:
            self.test_control = self.test_control_creator.create(self.driver)
        except ValueError as exc:
            return self.push_error(
                "Error while interpreting control: {}".format(exc)
            )

        # If test control tells us to skip the test, stop right here
        if self.test_control.skip:
            return self.push_skip(self.test_control.message)

        try:
            self.set_up()
            self.run()
            self.analyze()
            self.tear_down()
        except TestAbortWithError as exc:
            self.push_error(str(exc))
            return

    def push_success(self):
        """Set status to consider that the test passed."""
        # Given that we skip execution right after the test control evaluation,
        # there should be no way to call push_success in this case.
        assert not self.test_control.skip

        if self.test_control.xfail:
            self.result.set_status(TestStatus.XPASS)
        else:
            self.result.set_status(TestStatus.PASS)
        self.driver.push_result(self.result)

    def push_skip(self, message):
        """
        Consider that we skipped the test, set status accordingly.

        :param message: Label to explain the skipping.
        """
        self.result.set_status(TestStatus.SKIP, message)
        self.driver.push_result(self.result)

    def push_error(self, message):
        """
        Set status to consider that something went wrong during test execution.

        :param message: Message to explain what went wrong.
        """
        self.result.set_status(TestStatus.ERROR, message)
        self.driver.push_result(self.result)

    def push_failure(self, message):
        """
        Consider that the test failed and set status according to test control.

        :param message: Test failure description.
        """
        if self.test_control.xfail:
            status = TestStatus.XFAIL
            if self.test_control.message:
                message = "{} ({})".format(message, self.test_control.message)
        else:
            status = TestStatus.FAIL
        self.result.set_status(status, message)
        self.driver.push_result(self.result)

    def stdf(self):
        return stdf_in(self.test_dir())

    def latch_status(self):
        r = self.result
        pdump_to(
            self.stdf(),
            o=TC_status(
                passed=r.status in (TestStatus.PASS, TestStatus.XPASS),
                xfail=self.test_control.xfail,
                status=r.status.name,
                comment=self.test_control.message,
            ),
        )

    def latched_status(self):
        return pload_from(self.stdf())

    def _handle_info_for(self, path):
        """Return a string describing file handle information related to
        the provided PATH, such as the output of the "handle" sysinternal
        on Windows."""

        if sys.platform != "win32":
            return "No handle info on non-windows platform"

        # Adjust the provided path to something
        path = re.sub("^[a-zA-Z]:(.*)", r"\1", path).replace("/", "\\")

        gpdir = os.path.dirname(sys.executable)

        handle_path = os.path.abspath(
            os.path.join(
                gpdir,
                "Lib",
                "site-packages",
                "gnatpython",
                "internal",
                "data",
                "libexec",
                "x86-windows",
                "handle.exe",
            )
        )

        return Run([handle_path, "/AcceptEULA", "-a", "-u", path]).out

    def do_post_run_cleanups(self, ts_options):
        """Cleanup temporary artifacts from the testcase directory.
        Append removal failure info to the test error log. TS_OPTIONS
        are the testsuite command line options."""

        comments = []

        # In principle, most of this is the spawned test.py responsibilty,
        # because _it_ knows what it creates etc.  We have artifacts of our
        # own though (dump files for qualif runs, for example), and removing
        # these correctly can only be done from here. Doing the rest as well
        # is just simpler and more efficient.

        # Beware that some artifacts need to be preserved for qualification
        # runs. In particular, test execution logs and coverage reports which
        # might reside in temporary directories.

        # List of paths to filesystem entities we will want to remove, which
        # may hold file or directory names (to be removed entirely):
        cleanup_q = []

        def cleanup_on_match(subdirs, prefixes, parent):
            """
            Helper for the filesystem walking code below, to facilitate the
            processing of subdirectories. Queue for removal the subdirectories
            of ``parent`` in ``subdirs`` which have a name starting with one
            of the provided ``prefixes``, removing them from ``subdirs``.
            """
            to_clear = []
            for sd in subdirs:
                for prefix in prefixes:
                    if sd.startswith(prefix):
                        to_clear.append(sd)
                        break
            for sd in to_clear:
                cleanup_q.append(os.path.join(parent, sd))
                subdirs.remove(sd)

        # Perform a filesystem walk to craft the list of items we
        # can/should remove. Make it topdown so we can arrange not to
        # recurse within subirectories we cleanup as a whole.
        for dirpath, dirnames, filenames in os.walk(
            self.test_dir(), topdown=True
        ):
            # Nothing in "obj" dirs ever needs to be preserved
            cleanup_on_match(
                subdirs=dirnames, prefixes=["tmp", "obj"], parent=dirpath
            )

            # We can also always get rid of all the pure binary artifacts,
            # wherever they are produced. Files without extension, most often
            # executables, are considered never of interest.
            for fn in filenames:
                if (
                    fn.endswith(".trace")
                    or fn.endswith(".obj")
                    or fn.endswith(".o")
                    or fn.endswith(".exe")
                    or "." not in fn
                ):
                    cleanup_q.append(os.path.join(dirpath, fn))

            # Then for regular runs, we can remove test execution logs and the
            # scov test temp dirs as a whole. We can't remove these dirs in
            # qualification runs because they hold subcommand execution logs
            # and coverage reports which need to be preserved in qualification
            # packages.
            if not ts_options.qualif_level:
                for fn in filenames:
                    if fn == "test.py.log":
                        cleanup_q.append(os.path.join(dirpath, fn))

                cleanup_on_match(
                    subdirs=dirnames,
                    prefixes=["st_", "dc_", "mc_", "uc_"],
                    parent=dirpath,
                )

        # Deal with occasional removal failures presumably caused by stray
        # handles. Expand glob patterns locally, issue separate rm requests
        # for distinct filesystem entries and turn exceptions from rm into
        # test failures.
        for path in set(cleanup_q):
            try:
                rm(path, recursive=True)
            except Exception:
                handle_comment = self._handle_info_for(path)
                self.passed = False
                self.status = "RMFAILED"

                comments.append(
                    "Removal of %s failed\nHandle info follows:" % path
                )
                comments.append(handle_comment)

        with open(self.errf(), "a") as f:
            f.write("\n".join(comments))


class TestPyDriver(TestDriver):
    """
    Test driver that runs "test.py" scripts.
    """

    def add_test(self, dag):
        self.runner = TestPyRunner(
            self, self.result, self.test_dir(), self.working_dir()
        )
        self.add_fragment(dag, "run", self.runner.run_test)


class GroupPyDriver(TestDriver):
    """
    Test driver that runs "group.py" scripts.
    """

    def add_test(self, dag):
        # Generator of unique indexes for generated testcases
        indexes = itertools.count(1)

        # Run the "group.py" script, to generate testcases
        p = Run([sys.executable, "group.py"], cwd=self.test_dir(), timeout=20)
        if p.status != 0:
            raise RuntimeError(
                "Execution of group.py failed ({}):"
                "\nOutput:"
                "\n{}".format(self.test_dir("group.py"), p.out)
            )

        # Look for all "test.py" that were generated under this test directory
        for dirpath, _, filenames in os.walk(self.test_dir()):
            if "test.py" in filenames:
                self.add_test_py_run(dag, dirpath, next(indexes))

    def add_test_py_run(self, dag, test_dir, index):
        # Create a dedicated name for this generated test, with the same rules
        # as for regular tests.
        test_rel_dir = os.path.relpath(test_dir, self.test_dir())
        test_name = "{}-{}".format(
            self.test_name, unixpath(test_rel_dir).replace("/", "-")
        )

        # For debuggability, derive the group.py test environment
        test_env = dict(self.test_env)
        test_env["generated_test_dir"] = test_dir

        # Plan for a dedicated working directory
        working_dir = os.path.join(self.env.working_dir, test_name)

        result = TestResult(test_name, test_env)
        runner = TestPyRunner(self, result, test_dir, working_dir)
        self.add_fragment(dag, "run_{}".format(index), runner.run_test)


class GNATcovTestFinder(TestFinder):
    def probe(self, testsuite, dirpath, dirnames, filenames):
        # If we are running in qualification mode, punt if this test
        # is not within the subtrees attached to the requested level.
        qlevel = testsuite.main.args.qualif_level
        if qlevel and not QLEVEL_INFO[qlevel].hosts(dirpath):
            return None

        # If directory contains a "test.py" file *and* not a ".generated"
        # one, this this is a regular testcase.
        if "test.py" in filenames and ".generated" not in filenames:
            driver_cls = TestPyDriver

        # If it contains a "group.py" file, then this is a special test that
        # generates several actual testcases.
        elif "group.py" in filenames:
            driver_cls = GroupPyDriver

        # Otherwise, there is no testcase
        else:
            driver_cls = None

        if driver_cls:
            return ParsedTest(
                test_name=testsuite.test_name(dirpath),
                driver_cls=driver_cls,
                test_env={"testsuite_root_dir": testsuite.root_dir},
                test_dir=dirpath,
            )


# ===============
# == TestSuite ==
# ===============


class TestSuite(e3.testsuite.Testsuite):
    enable_cross_support = True

    @property
    def test_finders(self):
        return [GNATcovTestFinder()]

    def test_name(self, test_dir):
        # Start with a relative directory name from the tests subdirectory
        result = os.path.relpath(test_dir, self.test_dir)

        # We want to support running tests outside of the test directory, so
        # strip leading "..".
        pattern = os.path.pardir + os.path.sep
        while result.startswith(pattern):
            result = result[len(pattern) :]

        # Run some name canonicalization and replace directory separators with
        # dashes.
        result = unixpath(result).rstrip("/").replace("/", "-")

        # Tests from the internal testsuite used to be located in the "tests"
        # subdirectory. They are now in "../extra/tests", but we want the GAIA
        # name to remain the same.
        return strip_prefix("extra-", result)

    # --------------------------
    # -- GAIA file facilities --
    # --------------------------

    def _push_log(self, textlist, filename):
        """Append the list of lines in TEXTLIST to the GAIA log FILENAME."""

        # If there's nothing to push, return. Empty lists can show up here,
        # e.g. an empty dead list for a run with filter or stopped by
        # consecutive failures. We must be careful not to dump a possibly
        # invalid empty line in the output file in such a case.
        if textlist:
            with open(os.path.join(self.output_dir, filename), mode="a") as fd:
                fd.write("\n".join(textlist) + "\n")

    def _push_comments(self, textlist):
        self._push_log(textlist=textlist, filename="comment")
        self._comment_lines.extend(textlist)

    def _push_results(self, textlist):
        self._push_log(textlist=textlist, filename="results")

    def _push_altrun(self, textlist):
        self._push_log(textlist=textlist, filename="altrun")

    def _discriminants_log(self):
        return os.path.join(self.output_dir, "discs")

    def write_comment_file(self, f):
        f.write("\n".join(self._comment_lines) + "\n")

    # -------------------------------
    # -- STR production facilities --
    # -------------------------------

    def _init_strbox(self):
        """Initialize the directory where the STR production artifacts
        will be dropped."""
        self.strbox_dir = os.path.join(self.root_dir, QSTRBOX_DIR)
        mkdir(self.strbox_dir)

    def _dump_ctxdata(self):
        """Dump the testsuite context data file for use by the STR report
        producers."""

        if self.main.args.other_tool_info:
            (toolname, version_info) = Run(
                [sys.executable, self.main.args.other_tool_info], timeout=20
            ).out.split("##")

            other_tool_info = TOOL_info(exename=toolname, ver=version_info)
        else:
            other_tool_info = None

        jdump_to(
            CTXDATA_FILE,
            SUITE_context(
                runstamp=time_string_from(time.localtime()),
                host=host_string_from(self.env.host),
                treeref=treeref_at("."),
                cmdline=" ".join(sys.argv),
                options=OPT_info_from(options=self.main.args),
                gnatpro=TOOL_info(self.tool("gcc")),
                gnatemu=TOOL_info(self.tool("gnatemu")),
                gnatcov=TOOL_info("gnatcov"),
                other=other_tool_info,
            ),
        )

    # -----------------------
    # -- Common facilities --
    # -----------------------

    def tool(self, name):
        """Return tool name prefixed by target when in cross env.

        :param name: the tool name
        :type name: str
        :return: the final tool name
        :rtype: str
        """
        if self.env.is_cross:
            return self.env.target.triplet + "-" + name
        else:
            return name

    def _build_libsupport(self):
        args = self.main.args

        libsup_vars = []

        if self.env.is_cross:
            # Beware that the TARGET variable set here is used as the
            # --target switch to gprbuild calls from the Makefile, and
            # the args.target value we receive might have os-version
            # or machine extensions that aren't appropriate for that
            # (e.g. aarch64-elf,unknown).
            libsup_vars.append("TARGET={}".format(self.env.target.triplet))

        # The BOARD variable is used to fill the Emulator package Board
        # attribute, so convert the argument to a gnatemu name.
        board = control.gnatemu_board_name(args.board)
        if board:
            libsup_vars.append("BOARD={}".format(board))

        if args.RTS:
            libsup_vars.append("RTS={}".format(args.RTS))

        # QNX + run-cross2 will report an abort as an abnormal program
        # termination, which is not the point of the silent last chance
        # handler. QNX however does provide the exit function, so use that
        # instead.
        if args.target and ("qnx" in args.target or "morello" in args.target):
            libsup_vars.append("SILENT_LCH=exit")

        logfile = os.path.join(self.output_dir, "build_support.out")

        p = Run(
            ["make", "--debug", "-C", "support", "-f", "Makefile.libsupport"]
            + libsup_vars,
            output=logfile,
        )

        if p.status != 0:
            raise FatalError(
                ("Problem during libsupport construction. %s:\n" % logfile)
                + contents_of(logfile)
            )

    def set_up(self):
        """
        Prepare the testsuite run: compute and dump discriminants, run
        gprconfig and build the support library for the whole series of tests
        to come.
        """
        # First, perform a couple of validity checks on command-line arguments
        # and compute bits of internal state for later use from them.

        args = self.main.args

        # Enforce a default -gnat<version> for Ada, so each test can expect an
        # explicit setting to filter on. Expect an explicit one if we're
        # running for qualification, making sure we know what target language
        # we're qualifying for.

        def _get_canonical_ada_version(ada_ver: str) -> str:
            if ada_ver in ["83", "95"]:
                # For Ada 1983 and 1995, only -gnatYY is correct
                return ada_ver
            elif ada_ver in ["05", "2005"]:
                # For Ada 2005, use "05" to make sure all compilers get it.
                return "05"
            elif ada_ver[-2:] in ["12", "22"]:
                # Since Ada 2012, -gnatYY and -gnatYYYY are accepted
                # For Ada 2022, -gnatYYYY is mandatory
                return "20" + ada_ver[-2:]
            raise FatalError(f"Unable to retrieve Ada version from {ada_ver}")

        attr_cargs_ada = cargs_attr_for("Ada")
        cargs_ada = getattr(args, attr_cargs_ada)
        attr_cargs = cargs_attr_for("")
        cargs = getattr(args, attr_cargs)

        gnatcov_instr_ada_version = None
        qualifiable_ada_versions = ["95", "05", "12"]
        qav_str = "|".join(qualifiable_ada_versions)
        ada_version_match = re.match(
            r"-gnat(95|(?:20)?(?:05|12)|2022)", cargs_ada + cargs
        )

        if ada_version_match:
            ada_version = _get_canonical_ada_version(
                ada_version_match.groups()[0]
            )

            if (
                args.qualif_level
                and ada_version[-2:] not in qualifiable_ada_versions
            ):
                raise FatalError(
                    f"-gnat{ada_version} is invalid for qualification. "
                    f"Allowed versions are -gnat<{qav_str}>"
                )

            logging.debug(
                f"Setting Ada version to Ada {ada_version} from cargs"
            )
            gnatcov_instr_ada_version = ada_version
        else:
            if args.qualif_level:
                raise FatalError(
                    f"Missing -gnat<{qav_str}> in cargs:Ada for qualification"
                )
            else:
                logging.debug("Setting default Ada version to Ada 05")
                gnatcov_instr_ada_version = "05"
                setattr(args, attr_cargs_ada, cargs_ada + " -gnat05")

        # Most SPARK testcases require Ada 2022

        if args.spark_tests:
            logging.debug("Setting Ada version to Ada 2022 for SPARK")
            gnatcov_instr_ada_version = "2022"
            setattr(args, attr_cargs_ada, cargs_ada + " -gnat2022")

        # Make sure gnatcov instrument is called with the same Ada version as
        # the one passed to gnat afterwards
        args.ada_version = gnatcov_instr_ada_version

        # Expect an explicit -gnatec if we're running for qualification

        if args.qualif_level and "-gnatec" not in cargs_ada:
            raise FatalError("Missing -gnatec in cargs:Ada for qualification")

        # Source traces require the use of project files to convey units of
        # interest, which --gprmode will do:

        if args.trace_mode == "src":
            args.gprmode = True

        # On some targets, we need to link with -lgnat for any executable
        # to run and the toolchain doesn't do it automatically in some cases
        # (e.g. C only programs). This is a workaround:

        if args.target and "-elf" in args.target:
            args.largs += " -lgnat"

        # e3.testsuite forces the writing of the "comment" file at the end of
        # the testsuite run, but here we compute its content step by step. This
        # list will keep track of the lines to write.
        self._comment_lines = []

        # Initialize the GAIA log files. We need to do that before setting up
        # the altrun hooks, as their execution may dump output logs.
        for f in ("comment", "results", "discs", "altrun"):
            open(os.path.join(self.output_dir, f), "w").close()

        # Add current directory in PYTHONPATH, allowing Python testcase scripts
        # to find the SUITE and SCOV packages:
        self.env.add_search_path("PYTHONPATH", self.root_dir)

        # Generate the discs list for test.opt parsing, and dump the list in
        # a file that we can then to determine which discriminants were set
        # during a particular run:
        self.env.suite_discriminants = self._discriminants()
        self._push_log(
            textlist=[" ".join(self.env.suite_discriminants)],
            filename=self._discriminants_log(),
        )

        # Setup the alternate run hooks, if any
        self._setup_altrun_hooks()

        # Setup the STR box and dump the suite data file, for qualification
        # runs. Note that we must be in a revision controlled tree in this
        # case, so we can fetch a local reference for consistency comparisons.
        #
        # Dump the path to our python interpreter as well in this case, so the
        # STR generation shell script can find and reuse it. Use a simplified
        # format and location for this purpose (text, at toplevel).

        if self.main.args.qualif_level:
            self._init_strbox()
            self._dump_ctxdata()

            with open("python_bin.dump", "w") as f:
                f.write(sys.executable)

        # Dump useful comments about this run for starters

        self._push_comments(self._early_comments())

        # Resolve path argument values as needed while we have visibility on
        # where relative paths lead. Note that we're doing this after dumping
        # context data for STR purposes so this data leaves full paths
        # unexposed.

        self._resolve_paths()

        # Run the builder configuration for the testsuite as a whole. Doing
        # it here once both factorizes the work for all testcases and prevents
        # cache effects if PATH changes between testsuite runs.

        toolchain_discriminants = self._toolchain_discriminants()
        toolchain_discriminant = (
            toolchain_discriminants[0] if toolchain_discriminants else None
        )

        BUILDER.RUN_CONFIG_SEQUENCE(
            toplev_options=self.main.args,
            toolchain_discriminant=toolchain_discriminant,
        )

        # Build support library as needed

        self._build_libsupport()

        # Initialize counter of consecutive failures, to stop the run
        # when it is visibly useless to keep going.
        #
        # TODO: re-implement this feature
        self.n_consecutive_failures = 0

        self.maybe_exec(binfile=self.main.args.pre_testsuite, edir="...")

        # Make testsuite options and the discriminants file available from
        # testcases.
        self.env.main_options = args
        self.env.discr_file = self._discriminants_log()

    def tear_down(self):
        self.maybe_exec(binfile=self.main.args.post_testsuite, edir="...")

    # -----------------------------------
    # -- Early comments about this run --
    # -----------------------------------

    def _options_comment(self):
        return "Testsuite options:\n" + " ".join(
            quote_arg(arg) for arg in sys.argv[1:]
        )

    def _versions_comment(self):
        all_versions = [
            version("gnatcov") + ", " + version(self.tool("gnatls"))
        ]

        if self.main.args.target:
            all_versions.append(version(self.tool("gnatemu"), nlines=2))

        return "\n".join(["Running versions:"] + all_versions) + "\n"

    def _early_comments(self):
        return [
            "\n\n".join([self._options_comment(), self._versions_comment()])
        ]

    # -------------------------------
    # -- Discriminant computations --
    # -------------------------------

    def _discriminants(self):
        """Full set of discriminants that apply to this test"""

        return (
            self._base_discriminants()
            + self._board_discriminants()
            + self._qualif_level_discriminants()
            + self._cargs_discriminants()
            + self._rts_discriminants()
            + self._toolchain_discriminants()
            + self._gnatcov_discriminants()
            + self._rust_toolchain_discriminants()
        )

    def _base_discriminants(self):
        # List of toolchain discriminants for which we don't want to run
        # C++ tests, due to lack of compatibility with modern hosts.
        unsupported_cpp_toolchains = ["7.1.2", "5.04a1"]

        result = ["ALL"] + self.env.discriminants

        toolchain_discrs = self._toolchain_discriminants()

        # Only enable C++ tests if we have a C++ compiler that is not
        # blacklisted.
        if which(self.tool("g++")) and (
            not toolchain_discrs
            or toolchain_discrs[0] not in unsupported_cpp_toolchains
        ):
            result.append("C++")

        # Add a discriminant to track the current trace mode
        result.append(
            "src-traces"
            if self.main.args.trace_mode == "src"
            else "bin-traces"
        )

        if self.main.args.spark_tests:
            result.append("spark-tests")

        if self.main.args.all_warnings:
            result.append("all-warnings")

        if self.main.args.block:
            result.append("block")

        return result

    def _board_discriminants(self):
        """
        Compute a list of string discriminants that convey a request to run for
        a particular target board.
        """

        # There are two possible sources for this, with slightly different
        # operational meanings but which don't need to be differentiated
        # discriminant-wise. The board name and an indication that a specific
        # board was requested are good enough:

        boardname = (
            self.main.args.board
            if self.main.args.board
            else self.env.target.machine if self.env.target.machine else None
        )

        return ["board", boardname] if boardname else []

    def _cargs_discriminants(self):
        """
        Compute a list of discriminants (string) for each switch passed in all
        the --cargs command-line option(s).  The format of each discriminant
        CARGS_<X> where <X> is the switch stripped of its leading dashes.

        For instance, if this testsuite is called with --cargs='-O1'
        --cargs:Ada='-gnatp', then this function should return
        ['CARGS_gnatp', 'CARGS_O1'].

        Return an empty list if --cargs was not used.
        """

        allopts = " ".join(
            [
                getattr(self.main.args, attr)
                for attr in (
                    cargs_attr_for(lang)
                    for lang in [None] + control.KNOWN_LANGUAGES
                )
            ]
        )
        return ["CARGS_%s" % arg.lstrip("-") for arg in allopts.split()]

    def _qualif_level_discriminants(self):
        """
        List of single discriminant (string) denoting our current qualification
        mode, if any. This is ['XXX'] when invoked with --qualif-level=XXX, []
        otherwise.
        """

        return (
            []
            if not self.main.args.qualif_level
            else [self.main.args.qualif_level.upper()]
        )

    def _rts_discriminants(self):
        """
        Compute a list of discriminant strings that reflect the kind of runtime
        support library in use, as conveyed by the --RTS command-line
        option.
        """
        return _runtime_info(
            self.main.args.RTS, self.env.target.platform
        ).discrs

    def _toolchain_discriminants(self):
        """
        Compute the discriminant that reflects the version of the
        particular toolchain in use.
        """

        gcc_version = version(self.tool("gcc"))
        # Different version numbering for old GNAT versions
        m = re.search(r"GNAT Pro (\d\.\d\.\d)", gcc_version) or re.search(
            r"GNAT Pro (5.04a1)", gcc_version
        )

        return [m.group(1)] if m else []

    def _gnatcov_discriminants(self):
        """
        Compute the discriminant that reflects the version of the
        particular gnatcov in use.
        """
        gnatcov_info = control.gnatcov_info()

        return (
            ["gnatcov-{}".format(gnatcov_info.major())]
            if gnatcov_info.major()
            else []
        )

    def _rust_toolchain_discriminants(self):
        """
        Compute the discriminant that reflects the presence of a rust
        toolchain.
        """

        return ["rust"] if which("cargo") else []

    # --------------------------
    # -- Command-line options --
    # --------------------------

    def add_options(self, parser):
        parser.add_argument(
            "--post-run-cleanups",
            dest="do_post_run_cleanups",
            action="store_true",
            help="Request post-run cleanup of temporary artifacts.",
        )

        parser.add_argument(
            "--qualif-level",
            choices=list(QLEVEL_INFO),
            metavar="QUALIF_LEVEL",
            help="State we are running in qualification mode for a"
            " QUALIF_LEVEL target. This selects a set of applicable tests"
            " for that level.",
        )

        parser.add_argument(
            "--other-tool-info",
            dest="other_tool_info",
            metavar="OTHER_TOOL_INFO",
            default="",
            help="Name of a python script outputing other tool##version info",
        )

        # --pre|post family

        for when, what in control.ALTRUN_HOOK_PAIRS:
            parser.add_argument(
                "--%s" % altrun_opt_for(when, what),
                dest=altrun_attr_for(when, what),
                help="Run CMD %s to %s run" % (when, what),
                metavar="CMD",
            )

        parser.add_argument(
            "--altrun",
            metavar="ALTRUN_SUBDIR",
            help="Name of custom hooks directory.",
        )

        # Options shared with testcase scripts

        control.add_shared_options_to(parser, toplevel=True)

    # ---------------------
    # -- __resolve_paths --
    # ---------------------

    def _resolve_paths(self):
        """
        For options containing path values expressed possibly as relative from
        where the testsuite was launched, resolve to absolute paths.
        """

        # First deal with options accepting filenames per se:

        attributes_to_resolve = (
            ["kernel", "altrun"]
            + [
                altrun_attr_for(p0, p1)
                for p0, p1 in (
                    control.ALTRUN_HOOK_PAIRS + control.ALTRUN_GNATCOV_PAIRS
                )
            ]
            + [altrun_attr_for(p0) for p0 in control.ALTRUN_GPR]
        )

        for attr in attributes_to_resolve:
            current_value = getattr(self.main.args, attr)
            if current_value is not None:
                setattr(self.main.args, attr, os.path.abspath(current_value))

        # Then deal with compilation flags: turn -opt=possibly-relative-path
        # into -opt=absolute-path for relevant options.

        prefixes_to_resolve = ["-gnatec="]

        def resolve(carg):
            """
            CARG is one of the requested compilation flags. If it is meant to
            designate a file with a possibly relative path (e.g. -gnatec=),
            return the option modified to have an absolute path. Return the
            option unmodified otherwise.
            """

            carg_needs_resolution = any(
                carg.startswith(prefix) for prefix in prefixes_to_resolve
            )

            if carg_needs_resolution:
                (opt, path) = carg.split("=")
                return "=".join((opt, os.path.abspath(path)))
            else:
                return carg

        for lang in [None] + control.KNOWN_LANGUAGES:
            cargs_attr = cargs_attr_for(lang)
            current_cargs = getattr(self.main.args, cargs_attr).split()
            new_cargs = [resolve(carg) for carg in current_cargs]
            setattr(self.main.args, cargs_attr, " ".join(new_cargs))

    # -----------------------------
    # -- altrun hooks & friends --
    # -----------------------------

    def maybe_exec(self, binfile, args=None, edir=None):
        """
        Shortcut for the global maybe_exec. Log the result in the altrun log
        file, and abort the testsuite on failure.
        """
        if not binfile:
            return

        log = Log("")
        p = maybe_exec(log, binfile, args, edir)
        self._push_altrun(log.log.splitlines())
        exit_if(
            p.status != 0,
            "Altrun hook failed ({}):\n{}".format(binfile, p.out),
        )

    def _bin_for(self, base, indir=None):
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

        candidate_exe = base + (
            self.env.host.os.exeext if self.env.host.os.exeext else ""
        )
        candidates = [candidate_exe, base + ".py"]

        def relative_for(p):
            return p if indir is None else os.path.join(indir, p)

        programs = [p for p in candidates if os.path.exists(relative_for(p))]

        exit_if(
            len(programs) > 1,
            "from %s, too many binaries for %s:\n%s"
            % (os.getcwd(), base, "\n".join(programs)),
        )
        return programs[0] if programs else None

    def _setup_altrun_hooks(self):
        """Finalize the altrun/<subdir> for this run, if any, and install the
        pre/post hooks available from there."""

        if not self.main.args.altrun:
            return

        ctldir = self.main.args.altrun

        # Run the altrun subdir setup code, if any, then check each possible
        # hook of interest. Switch to the local directory for the setup step,
        # designed for local operations:

        self.maybe_exec(
            binfile=self._bin_for("setup", indir=ctldir), edir=ctldir
        )

        def install_altrun_for(p0, p1=None, binbase=None):
            """Establish an implicit value for the --P0_P1 command line option
            if we find a matching binary program in the altrun subdir we are
            processing. BINBASE provides the binary base name to use."""

            binfile = self._bin_for(os.path.join(ctldir, binbase))

            if not binfile:
                return

            attr = altrun_attr_for(p0, p1)
            exit_if(
                getattr(self.main.args, attr),
                "%s altrun conflicts with explicit --%s" % (binfile, attr),
            )

            self._push_altrun(["hooking %s to %s" % (attr, binfile)])
            setattr(self.main.args, attr, binfile)

        # For the toplevel testsuite driver hooks, map on binaries
        # matching the command line option names:

        for when, what in control.ALTRUN_HOOK_PAIRS:
            install_altrun_for(
                p0=when, p1=what, binbase=altrun_opt_for(when, what)
            )

        # For the gnatcov <command> replacements, map on binaries called
        # 'c<command>':

        for pgm, cmd in control.ALTRUN_GNATCOV_PAIRS:
            install_altrun_for(p0=pgm, p1=cmd, binbase="c%s" % cmd)

        # For the gpr<tool> replacements, map on binaries called cgpr<tool>:
        for pgm in control.ALTRUN_GPR:
            install_altrun_for(p0=pgm, p1=None, binbase=f"c{pgm}")


if __name__ == "__main__":
    sys.exit(TestSuite().testsuite_main())
