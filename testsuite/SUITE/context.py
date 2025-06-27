"""Testsuite context.

This module is imported by all testcases. It exposes a single "thistest"
instance of a Test class, to switch to the test subdir, and offer command line
and test status management facilities.

It also exposes a few global variables of general use (env, TEST_DIR, etc)
"""

import logging
import os
import re
import sys
import time

from e3.diff import diff
from e3.fs import rm
from e3.main import Main
from e3.os.fs import cd
from e3.os.process import Run
from e3.testsuite.driver.diff import (
    CanonicalizeLineEndings,
    LineByLine,
    OutputRefiner,
    RefiningChain,
)

from SUITE import control
from SUITE.control import GPRCLEAN, env
from SUITE.cutils import (
    exit_if,
    indent,
    indent_after_first_line,
    lines_of,
    ndirs_in,
)


# This module is loaded as part of a Run operation for a test.py
# file found and launched by the toplevel driver

logger = logging.getLogger("SUITE.context")

# This is where the toplevel invocation was issued for the individual test
# at hand, which we expect to be where the toplevel testsuite.py is located.
ROOT_DIR = os.getcwd()

# Internal helper to dispatch information to test.py.err/log/out


class _ReportOutput(object):
    """
    A class that allows us to write some text to a report file, while
    bufferizing part of it until we know whether this part should also be
    printed on standard output or not.  The idea is to buffer the output
    generated for each driver until the end of the test, and then print that
    output to stdout if we then determine that the test failed.

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
        """
        Turn printing of the output buffer on.  The printing will be done at
        the next flush.
        """
        self.print_diff = True

    def log(self, text, end_of_line=True):
        """
        Write the given text in the output file.  This also adds the text to
        the output buffer.

        PARAMETERS
          text:   The text to be logged.
          end_of_line: If True, then append a '\n' character at the end
                  of text. This affects both the report file and the output
                  buffer. The idea is to emulate the "print" statement
                  which adds this '\n' by default too.
        """
        if end_of_line:
            text += "\n"
        self.output += text
        self.report_fd.write(text)

    def flush(self):
        """
        Reset the output buffer (printing its content on standard output first
        if print_diff is True).  Reset print_diff to False as well.
        """
        if self.print_diff:
            sys.stdout.write(self.output + " ")
        self.output = ""
        self.print_diff = False
        self.report_fd.flush()

    def close(self):
        """Close the file descriptor for our report file."""
        self.report_fd.close()


class Test(object):
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

    def __init__(self):
        """
        Initialize the instance: switch to the test subdirectory, parse command
        line options, reset the failures counter and precompute gprbuild
        options we'll have to pass on every call to convey config options.
        """
        # Shortcut for SUITE.control.env
        self.env = env

        self.start_time = time.time()

        # Compute this test's home directory, absolute dir where test.py
        # is located, then the position relative to testsuite.py. Note that
        # .__file__ might be an absolute path, which os.path.join handles
        # just fine (join("/foo", "/foo/bar") yields "/foo/bar").

        testsuite_py_dir = ROOT_DIR
        test_py_dir = os.path.dirname(sys.modules["__main__"].__file__)

        self.homedir = os.path.join(testsuite_py_dir, test_py_dir)
        self.reldir = os.path.relpath(self.homedir, start=testsuite_py_dir)

        # Perform a simple canonicalization of the relative dir to simplify
        # related computations/assumptions in other parts of the testsuite.
        self.reldir = self.reldir.replace("\\", "/")

        # Compute the depth of this test wrt testsuite root.
        self.depth = ndirs_in(os.path.relpath(self.homedir, ROOT_DIR))

        cd(self.homedir)

        self.options = self.__cmdline_options()
        self.n_failed = 0
        self.report = _ReportOutput(self.options.report_file)
        self.current_test_index = 0

        # Whether this test will be using project files to locate SCOs when
        # running gnatcov.  This is decided on a per gnatcov invocation basis.
        # self.options.gnatcov states whether we're queried to do this for
        # SCOV driven tests.
        self.gprmode = False

        # Callgrind may be invoked more than once for each test. Memorize the
        # number of times it has been used in order to generate multiple logs.
        self.callgrind_count = 0

        # By default, hide the warning that says that non-instrumented native
        # coverage is deprecated, as it would make all tests fail. Testcases
        # that check the presence of this warning can just remove this
        # environment variable.
        os.environ["GNATCOV_NO_NATIVE_WARNING"] = "1"

    def cleanup(self, project, options):
        """Cleanup possible remnants of previous builds."""
        Run([GPRCLEAN, "-P%s" % project] + options)
        rm("*.xcov")
        rm("*.bin")

    # Test status management

    def log(self, text, new_line=True):
        """Calls self.report.log."""
        self.report.log(text, new_line)

    def flush(self):
        """Calls self.report.flush."""
        self.report.flush()

    def comment(self, text):
        """Output a TEXT comment."""
        self.log("  - %s." % indent_after_first_line(text, "    "))

    def failed(self, comment="assertion failed"):
        """Register a check failure."""
        self.log("  * %s" % indent_after_first_line(comment, "    "))
        self.report.enable_diffs()
        self.n_failed += 1

    def fail_if(self, expr, comment="assertion failed"):
        """Register a check failure when EXPR is true."""
        if expr:
            self.failed(comment)

    def fail_if_not_equal(self, what, expected, actual):
        """Register a check failure when EXPECTED and ACTUAL are not equal."""
        self.fail_if(
            expected != actual,
            "Unexpected {}. Expected:"
            "\n{}"
            "\nBut got:"
            "\n{}".format(what, indent(str(expected)), indent(str(actual))),
        )

    def _fail_if_regex(self, what, regexp, actual, match_is_fail=False):
        if isinstance(regexp, str):
            regexp = re.compile(regexp)
        # Canonicalize to Unix-style line endings to have cross-platform checks
        actual = actual.replace("\r\n", "\n")
        matching = bool(regexp.match(actual))
        self.fail_if(
            matching == match_is_fail,
            "Error {}."
            "\n{}:"
            "\n{}"
            "\nBut got:"
            "\n{}".format(
                what,
                "Unexpected" if match_is_fail else "Expected",
                indent(regexp.pattern),
                indent(str(actual)),
            ),
        )

    def fail_if_no_match(self, what, regexp, actual):
        """Register a check failure when ACTUAL does not match regexp."""
        self._fail_if_regex(
            what,
            regexp,
            actual,
            match_is_fail=False,
        )

    def fail_if_match(self, what, regexp, actual):
        """Register a check failure when ACTUAL does match regexp."""
        self._fail_if_regex(
            what,
            regexp,
            actual,
            match_is_fail=True,
        )

    def stop(self, exc):
        self.failed("Processing failed")
        self.result()
        raise exc

    def stop_if(self, expr, exc):
        if expr:
            self.stop(exc)

    def report_output_refiners(self):
        """
        When using the diff computing API (e.g. fail_if_diff), one needs to
        refine the actual output to ignore execution specific data (such as
        absolute paths, timestamps etc.). This function returns output refiners
        for gnatcov reports.

        TODO: this only implement refiners for xcov[+]?. Implement for other
        report formats when needed.
        """

        class XcovPathRemover(OutputRefiner):
            """Remove path lines in outputs."""

            def refine(self, output):
                if os.path.exists(output.strip("\n:")):
                    return ""
                return output

        return [
            # Refiners for the xcov report
            LineByLine(XcovPathRemover()),
            # Ignore platform specificities
            CanonicalizeLineEndings(),
        ]

    def fail_if_diff_internal(
        self,
        baseline_file,
        actual,
        failure_message="unexpected output",
        output_refiners=None,
        ignore_white_chars=True,
    ):
        """Compute the diff between expected and actual outputs.

        Return an empty list if there is no diff, and return a list that
        contains an error message based on ``failure_message`` otherwise.

        :param baseline_file: Absolute filename for the text file that contains
            the expected content (for baseline rewriting, if enabled), or None.
        :param actual: Actual content to compare.
        :param failure_message: Failure message to return if there is a
            difference.
        :param output_refiners: List of refiners to apply both to the baseline
            and the actual output. Refer to the doc in
            ``e3.testsuite.driver.diff``. If None, use
            ``self.report_output_refiners()``.
        :param ignore_white_chars: See ``e3.diff``.
        """

        if output_refiners is None:
            output_refiners = self.report_output_refiners()

        # Run output refiners on the actual output, and on the baseline
        refiners = RefiningChain(output_refiners)
        refined_actual = refiners.refine(actual)
        with open(baseline_file, "r") as f:
            refined_baseline = refiners.refine(f.read())

        # Get the two texts to compare as list of lines, with trailing
        # characters preserved (splitlines(keepends=True)).
        expected_lines = refined_baseline.splitlines(True)
        actual_lines = refined_actual.splitlines(True)

        # Compute the diff. If it is empty, return no failure. Otherwise,
        # include the diff in the test log and return the given failure
        # message.
        d = diff(
            expected_lines,
            actual_lines,
            ignore_white_chars=ignore_white_chars,
        )
        if not d:
            return []

        message = failure_message

        # If requested and the failure is not expected, rewrite the test
        # baseline with the new one.
        if baseline_file is not None and self.options.rewrite:
            with open(baseline_file, "w") as f:
                f.write(refined_actual)
            message = "{} (baseline updated)".format(message)

        # Send the appropriate logging.
        self.failed(
            "Diff failure: {}\n".format(message) + "\n{}".format(d) + "\n"
        )

    def fail_if_diff(
        self,
        baseline_file,
        actual_file,
        failure_message="unexpected output",
        output_refiners=None,
        ignore_white_chars=True,
    ):
        """
        Wrapper around fail_if_diff_internal, taking an actual_file parameter
        instead of an actual string.
        """
        with open(actual_file, "r") as f:
            self.fail_if_diff_internal(
                baseline_file,
                f.read(),
                failure_message,
                output_refiners,
                ignore_white_chars,
            )

    def result(self):
        """Output the final result which the testsuite driver looks for.

        This should be called once at the end of the test
        """
        from SUITE.tutils import run_processes

        if self.n_failed == 0:
            self.log("==== PASSED ============================.")
        else:
            self.log("**** FAILED ****************************.")

        # Log the total execution time as well as the list of processes that
        # were run, with their duration. This is useful to investigate where
        # time is spent exactly when testcases take too long to run.
        duration = time.time() - self.start_time
        logger.debug("Total ellapsed time: {:.3f}s".format(duration))
        if run_processes:
            logger.debug("Processes run:")
            for p in run_processes:
                logger.debug(
                    "  [{:6.3f}s] {}".format(
                        p.duration, " ".join(p.original_cmd)
                    )
                )

        # Flush the output, in case we forgot to do so earlier.  This has no
        # effect if the flush was already performed.
        self.flush()
        self.report.close()

    def create_callgrind_id(self):
        """Return a test-unique ID to identify a callgrind run."""
        self.callgrind_count += 1
        return self.callgrind_count

    # Test options management

    def __cmdline_options(self):
        """Return an options object to represent the command line options"""
        main = Main(platform_args=True)
        parser = main.argument_parser
        parser.add_argument("--timeout", type=int, default=None)
        parser.add_argument(
            "--report-file",
            metavar="FILE",
            help="The filename where to store the test report" " [required]",
        )
        parser.add_argument(
            "--qualif-level",
            metavar="QUALIF_LEVEL",
            help="The target qualification level when we are"
            " running in qualification mode.",
        )

        parser.add_argument(
            "--xcov-level",
            help="Force the --level argument passed to xcov"
            "instead of deducing it from the test"
            " category when that normally happens.",
        )

        parser.add_argument("--tags", default="")

        parser.add_argument(
            "--ada-version",
            help="Ada version that is passed to gnatcov instrument",
            default="",
        )

        control.add_shared_options_to(parser, toplevel=False)

        main.parse_args()

        # "--report-file" is a required "option" which is a bit
        # self-contradictory, but it's easy to do it that way.
        exit_if(
            main.args.report_file is None,
            "The report file must be specified with --report-file",
        )

        # Get our tags set as a list. Fetch contents from file if needed
        # first:
        if main.args.tags and main.args.tags.startswith("@"):
            main.args.tags = " ".join(lines_of(main.args.tags[1:]))
        if main.args.tags:
            main.args.tags = main.args.tags.split()

        return main.args

    def suite_cargs_for(self, lang):
        """
        String of options passed as --cargs[:LANG] to the testsuite driver.
        None if no such option passed. LANG might be None, to fetch options
        passed as --cargs.
        """
        return getattr(thistest.options, control.cargs_attr_for(lang))

    def suite_covpgm_for(self, cmd):
        """
        Alternate program to launch in lieu of "gnatcov CMD", if any specified
        with the --gnatcov-CMD= command line option. None otherwise.
        """
        return getattr(thistest.options, "gnatcov_%s" % cmd, None)

    def suite_gprpgm_for(self, pgm):
        """
        Alternate program to launch in lieu of "gpr<tool>",
        """
        return getattr(thistest.options, pgm, None)

    def support_dir(self):
        return os.path.join(ROOT_DIR, "support")

    def bits(self):
        """
        Address size, in bits, for the programs gnatcov is given to analyze
        this run.
        """
        return env.target.cpu.bits


# Instantiate a Test object for the individual test module that imports us
thistest = Test()

# Allow import of a common "test_support" module from test.py when
# there is a test_support.py available uptree.
__parent_dir = os.path.dirname(os.getcwd())
if os.path.exists(os.path.join(__parent_dir, "test_support.py")):
    sys.path.append(__parent_dir)
