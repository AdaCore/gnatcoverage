# ****************************************************************************
# **                         TestCase Control abstractions                  **
# ****************************************************************************

from SUITE.gprutils import gprcov_for

# =================================
# == User Level Coverage Control ==
# =================================

# Facility to allow control of the operations performed by the source
# coverage oriented tests driven by the tc.TestCase class.
#
#   - Influence the project file constructed to build the testcase binaries,
#     with dependencies to other projects or directives pertaining to the
#     Coverage package within the project (deps).
#
#   - State the set of units for which we expect reports as result. The
#     testsuite driver will then check that we do get the expected reports and
#     only those. For those that are expected, the testsuite performs the
#     standard checks of matching outcome with expectations stated in drivers
#     (units_in, units_out, ulist_in, ulist_out).
#
#   - Influence the GPR related command line options to gnatcov, controlling
#     which projects are to be considered part of the analysis closure
#     (gprsw).
#
#   - Influence the non-GPR related command line options to gnatcov coverage
#     (covoptions).

class CovControl:

    def __init__ (
        self, deps = (), units_in = None, ulist_in = None,
        units_out = None, ulist_out = None, xreports = None,
        gprsw = None, covoptions=""):

        # To control "with" dependencies (set of projects that will be withed
        # by the one we will be generating for the testcase):

        self.deps = list(deps)

        # A GPRswitches instance to hold the set of GPR options to pass
        # to gnatcov to convey units of interest.

        self.gprsw = gprsw

        # Extra options to gnatcov coverage only, typically for "-S" variants.

        self.covoptions = covoptions

        # To control Units related attributes in the GPR Coverage package:

        self.units_in = units_in
        self.units_out = units_out
        self.ulist_in = ulist_in
        self.ulist_out = ulist_out

        # To instruct the testsuite driver about the set of source reports we
        # expect.

        # None is taken to mean "we don't expect anything in particular" as in
        # "we don't care", so anything is expected. An empty list is taken to
        # mean "we expect no report at all", so anything that comes out is
        # unexpected.

        self.xreports = xreports

    # ------------------
    # -- [un]expected --
    # ------------------

    def unexpected (self, source):
        return (
            self.xreports is not None
            and source not in self.xreports)

    def expected (self, source):
        return not self.unexpected (source)

    # ------------------
    # -- requires_gpr --
    # ------------------

    def requires_gpr (self):
        """Whether honoring this control object implictly requires the use of
        a project file."""

        return (self.deps
                or self.gprsw
                or self.units_in
                or self.units_out
                or self.ulist_in
                or self.ulist_out)

    # ---------
    # -- gpr --
    # ---------

    def gpr (self):

        """The GPR Coverage package corresponding to our lists of attribute
           contents, as a multiline string."""

        return gprcov_for (
            units_in = self.units_in,
            units_out = self.units_out,
            ulist_in = self.ulist_in,
            ulist_out = self.ulist_out
            )

# =======================
# == Testcase Category ==
# =======================

# A specific category tells the testsuite engine which coverage notes are
# relevant for a testcase execution.

class _Category:
    def __init__(self, name):
        self.name = name

# This is the set of valid categories. Note that some testcases such as
# those checking the conformance of the output report format, pertain to no
# category in particular.

class CAT:

    # Set of criteria related categories

    stmt = _Category (name = "stmt")
    decision = _Category (name = "decision")
    mcdc = _Category (name = "mcdc")

    critcats = (stmt, decision, mcdc)

    # Special value asking to determine the category from the Testcase
    # location (relative pathname from the testsuite root)

    auto = _Category (name = "auto")

