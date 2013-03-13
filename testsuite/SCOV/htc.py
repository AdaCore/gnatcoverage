# ****************************************************************************
# ** HARNESS TESTCASE abstraction **
# ****************************************************************************

# This module exposes the HarnessTestCase class, designed to automate all the
# processing required to run a source coverage qualification testcase setup
# where test.py was found, but without requiring that subprocesses (xcov, xrun,
# ...) finish without error.

# The aim is to verify that the testsuite detects mismatch between tests
# expectations and actual coverage results.

# ****************************************************************************

import re

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.cutils import contents_of
from SUITE.tutils import thistest


# =======================
# == HarnessDiagnostic ==
# =======================

# A testcase "error" text reported or expected in the Harness output.

class HarnessDiagnostic:
    """
    A testcase "error" text reported or expected in the HarnessTestCase output.
    """

    def __init__(self, text):
        self.text = text
        self.nmatches = 0

    def match(self, reported):
        """
        Return if self (an expected diagnostic) matches the given reported one.
        """
        return self.text in reported.text


# =====================
# == HarnessTestCase ==
# =====================

class HarnessTestCase(TestCase):

    def __init__(
        self,
        expected_diags,
        extradrivers="", extracargs="", category=CAT.auto
    ):
        TestCase.__init__(self, extradrivers, extracargs, category)

        # By default, these test cases expect failures from subprocesses.
        self.expect_failures = True

        self.expected_diags = expected_diags

    def __count_match_on(self, reported, expected):
        reported.nmatches += 1
        expected.nmatches += 1
        thistest.n_failed -= 1

    def run(self):
        TestCase.run(self)

        thistest.flush()

        self.reported_diags = [
            HarnessDiagnostic (text = errtext)
            for errtext in re.findall (
                pattern = "^  \* (?:exception|\(.*\)) .*",
                string  = contents_of (thistest.report.report_file),
                flags   = re.M
                )
            ]

        [self.__count_match_on (reported, expected)
         for reported in self.reported_diags for expected in self.expected_diags
         if expected.match(reported)]

        [thistest.fail_if (
                expected.nmatches != 1,
                "expectation check failed - %s" % expected.text
                )
         for expected in self.expected_diags]

        for i in self.reported_diags:
            print i.__dict__

        [thistest.fail_if (
                reported.nmatches != 1,
                "reporting check failed - %s" % reported.text)
         for reported in self.reported_diags]
