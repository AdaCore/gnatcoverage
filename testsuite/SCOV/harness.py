# ****************************************************************************
# ** HARNESS related abstractions **
# ****************************************************************************

# This module exposes the HarnessDiagnostic and HarnessMonitor classes to
# allow self-testing the harness behavior on testcases, verifying that mis-
# stated expectations or unexpected coverage violations are reported as
# expected in the harness execution reports.

# ****************************************************************************

import re
from SUITE.cutils import contents_of
from SUITE.tutils import thistest

# -----------------------
# -- HarnessDiagnostic --
# -----------------------

# A testcase "error" text reported or expected in the Harness output.

class HarnessDiagnostic:
    def __init__ (self, text):
        self.text = text
        self.nmatches = 0

# --------------------
# -- HarnessMonitor --
# --------------------

# The harness monitor, which compares a list of expected diagnostics
# in the harness output with the actual current output.

class HarnessMonitor:
    def __init__ (self, expected_diags):
        self.expected_diags = expected_diags

    def __count_match_on (self, reported, expected):
        reported.nmatches += 1
        expected.nmatches += 1

    def run (self):

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
         if expected.text in reported.text]

        thistest.n_failed -= len (self.expected_diags)

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
