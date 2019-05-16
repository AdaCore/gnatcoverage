# ****************************************************************************
# ** TESTCASE abstraction **
# ****************************************************************************

# This module exposes the TestCase class, designed to automate all the
# processing required to run a source coverage qualification testcase setup
# where test.py was found.
#
# A typical use, using default parameters, is TestCase_Here().run().

# Roughly:
#
# - The instance initialization locates the  applicable test_ drivers, then
#
# - run() exercises them all (for each: build, emulate, analyze, match reports
#   vs expectations), and evaluates associated consolidation requests, if any.

# The class facilities may be used for regression tests as well, not part of
# the qualification database.

# This module also exposes ...
from SUITE.context import thistest

# allowing calls to thistest.result() straight after run(), without requiring
# a dedicated import on the client side

# ****************************************************************************

import os, re
import os.path

from SUITE.context import TEST_DIR
from SUITE.cutils import to_list, contents_of, FatalError

from SUITE.qdata import Qdata, QDentry

from gnatpython.fileutils import ls

from internals.driver import SCOV_helper_bin_traces, SCOV_helper_src_traces
from internals.driver import WdirControl

from SCOV.tctl import CAT, CovControl

# ==============
# == TestCase ==
# ==============

class TestCase:

    # ==========================
    # == Helpers for __init__ ==
    # ==========================

    def __expand_drivers(self, patterns):
        """Add to the list of drivers to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""

        [self.all_drivers.extend (ls(p)) for p in to_list(patterns)]

    def __expand_cspecs(self, patterns):
        """Add to the list of consolidation specs to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""

        [self.all_cspecs.extend (ls(p)) for p in to_list(patterns)]

    def __expand_shared_controllers(self, drivers, cspecs):
        """Search and expand possible shared drivers and/or consolidation
        specs uptree for our local functional units."""

        # shared drivers would be <updir>/test_<xx>*.(adb|c) for some possible
        # updir and every <xx> such that there is a src/<xx>*.(adb|c).

        # Likewise for consolidation specs, as <updir>/cons_<xx>*.txt

        # Gather *set* of <xx> candidates first, then expand the associated
        # possible lists of drivers (each maybe empty). Beware not to include
        # child or sub units, as these dont mirror as such in the set of test
        # drivers.

        sxx = set(srcmatch.group(1)
                  for srcmatch in (re.match ("([a-z_]*).*\.(adb|c)",
                                             os.path.basename(src))
                                   for src in ls ("src/*"))
                  if srcmatch)

        # If there is no candidate body in src/, arrange to run all the
        # drivers. This is useful for test groups on GPR variations for
        # example, where we typically want to run all the drivers and check
        # the analysis results against different sets of SCOS.

        if len(sxx) == 0:
            sxx = [""]

        for body in sxx:
            for prefix in ("../" * n for n in range(1, thistest.depth)):
                if drivers: self.__expand_drivers (
                    "%(p)ssrc/test_%(b)s*.adb %(p)ssrc/test_%(b)s*.c"
                    % {'p' : prefix, 'b' : body}
                    )
                if cspecs: self.__expand_cspecs (
                    "%ssrc/cons_%s*.txt" % (prefix, body)
                    )

    def __category_from_dir(self):
        """Compute test category from directory location."""

        for cat in CAT.critcats:
            if re.search ("/%s(/|$)" % cat.name, TEST_DIR):
                return cat

        raise FatalError(
            "Unable to infer test category from subdir '%s'" % TEST_DIR)

    def __drivers_from(self, cspec):
        """Compute the set of drivers that need to be combined for
        consolidation purposes, extracted from the consolidation spec in
        CSPEC."""

        # Extract the drivers regular expression to match from the
        # consolidation spec file, ...

        drv_expr = re.match ("drivers=(.*)", contents_of (cspec)).group(1)

        # ... then construct and return the retricted list of drivers that
        # match this expression

        return [drv for drv in self.all_drivers if re.search (drv_expr, drv)]

    # ==============
    # == __init__ ==
    # ==============

    def __init__(self, extradrivers="", extracargs="", category=CAT.auto):

        # By default, these test cases expect no error from subprocesses (xrun,
        # xcov, etc.)
        self.expect_failures = False

        # Step 1: Compute the list of drivers and consolidation specs
        #         to exercise
        # -----------------------------------------------------------

        # Drivers first. Probe all those from src/ plus those explicitely
        # provided. If that is empty, see if we have bodies aimed at being
        # exercised by common drivers up-tree. Abort if there's nothing to
        # exercise at all

        self.all_drivers = []
        self.__expand_drivers("src/test_*.adb src/test_*.c " + extradrivers)

        if len(self.all_drivers) == 0:
            self.__expand_shared_controllers(drivers=True, cspecs=False)

        thistest.stop_if (
            len(self.all_drivers) == 0,
            FatalError ("Request to exercise empty test_set"))

        # Seek consolidation specs, then. Similar scheme, local check first,
        # then seek shared entities

        self.all_cspecs = []
        self.__expand_cspecs("src/cons_*.txt")

        if len(self.all_cspecs) == 0:
            self.__expand_shared_controllers(drivers=False, cspecs=True)


        # Step 2: Determine a few test parameters common to all drivers
        # -------------------------------------------------------------

        # - test category:

        # If automatic detection from subdir was requested, do that.
        # Otherwise, use the provided argument, which might be None or a
        # criterion related value.

        self.category = (
            self.__category_from_dir() if category == CAT.auto else category)

        # - extra compilation arguments, added to what --cargs was provided to
        #   the testsuite command line:

        self.extracargs = extracargs

        # Step 3: Setup qualification data for this testcase
        # --------------------------------------------------

        self.qdata = Qdata(tcid=TEST_DIR)

    # =====================
    # == Helpers for run ==
    # =====================

    def __xcovlevels (self):
        """Compute the list of --level values to exercise"""

        # If we have an explicit level query, use that. Fallback to defaults
        # otherwise. When there are multiple levels for a category, e.g. mcdc
        # variants, pick the first one only in qualification mode.

        if thistest.options.xcov_level:
            return [thistest.options.xcov_level]

        default_xcovlevels_for = {
            # Tests without categories should be ready for anything.
            # Exercise with the strictest possible mode:
            None: ["stmt+mcdc"],

            CAT.stmt:     ["stmt"],
            CAT.decision: ["stmt+decision"],
            CAT.mcdc:     ["stmt+mcdc", "stmt+uc_mcdc"]}

        defaults = default_xcovlevels_for [self.category]
        return (
            [defaults[0]] if thistest.options.qualif_level else defaults)

    def __register_qde_for (self, drvo):
        """Register a qualif data entry for driver object DRVO, about to
        be executed"""

        self.qdata.register (
            QDentry (xfile=drvo.xfile,
                     drivers=drvo.drivers, xrnotes=drvo.xrnotes,
                     wdir=os.path.normpath(drvo.awdir())
                     )
            )

    # Base prefix for Working directories, per --level. Shared across
    # runs for multiples levels:

    _wdbase_for = {
        "stmt":          "st_",
        "stmt+decision": "dc_",
        "stmt+mcdc":     "mc_",
        "stmt+uc_mcdc":  "uc_"
        }

    def __run_one_covlevel(self, covlevel, covcontrol, subdirhint):
        """Run this testcase individual drivers and consolidation tests
        with --level=COVLEVEL, using the provided COVCONTROL parameters and
        requesting SUBDIRHINT to be part of temp dir names."""

        this_scov_helper = (
            SCOV_helper_bin_traces if thistest.options.trace_mode == 'bin'
            else SCOV_helper_src_traces if thistest.options.trace_mode == 'src'
            else None)

        # Compute the Working directory base for this level, then
        # run the test for each indivdual driver:

        this_wdbase = self._wdbase_for [covlevel]

        wdctl = WdirControl (
            wdbase = this_wdbase, bdbase = self._available_bdbase,
            subdirhint = subdirhint)

        for driver in self.all_drivers:
            drvo = this_scov_helper (self, drivers=[driver],
                                     xfile=driver,
                                     xcovlevel=covlevel, covctl=covcontrol,
                                     wdctl=wdctl)
            self.__register_qde_for (drvo)
            drvo.run()

        # Now we have a common binary dir prefix to reuse

        if not self._available_bdbase:
            self._available_bdbase = this_wdbase

        # Next, run applicable consolidation tests.

        wdctl = WdirControl (
            wdbase = this_wdbase, bdbase = self._available_bdbase,
            subdirhint = subdirhint)

        for cspec in self.all_cspecs:
            drvo = this_scov_helper (self, drivers=self.__drivers_from(cspec),
                                     xfile=cspec,
                                     xcovlevel=covlevel, covctl=covcontrol,
                                     wdctl=wdctl)
            self.__register_qde_for (drvo)
            drvo.run()

    # =========
    # == run ==
    # =========

    def run(self, covcontrol = None, subdirhint = ""):
        """Execute this testcase, using coverage configuration
        parameters from COVCONTROL and SUBDIRHINT prepended to each
        driver id in temp subdir names."""

        # We have exception processing below, which needs to operate
        # in the test root dir.

        homedir = os.getcwd()

        try:

            # Run the set of drivers and consolidation tests for each
            # appropriate xcovlevel. Arrange to reuse the binary subdir of the
            # first run in subsequent ones. For single-level runs, having
            # the binaries together with the other artifacts is convenient.

            self._available_bdbase = None

            [self.__run_one_covlevel (
                    covlevel=covlevel, covcontrol=covcontrol,
                    subdirhint=subdirhint)
             for covlevel in self.__xcovlevels()]

        finally:

            # If we are running for qualification purposes, dump data needed
            # for qualification test-results production purposes. try/finally
            # is critical in making sure we dump results in case of failure
            # with exception as well.

            # This must be done in the test root dir and an exception might
            # have left us anywhere, so ...

            os.chdir (homedir)

            if thistest.options.qualif_level:
                self.qdata.flush()
