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

from SUITE.cutils import to_list, contents_of, FatalError
from SUITE.tutils import TEST_DIR

from SUITE.qdata import Qdata, QDentry

from gnatpython.fileutils import ls

from internals.driver import SCOV_helper

# ==============
# == TestCase ==
# ==============

class TestCase:

    def __expand_drivers(self, patterns):
        """Add to the list of drivers to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""

        [self.all_drivers.extend (ls(p)) for p in to_list(patterns)]

    def __expand_shared_drivers(self):
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

        [self.__expand_drivers (
                "%ssrc/test_%s*.adb" % (prefix, body))
         for prefix in ("../" * n for n in range(1, 3))
         for body in sxx]

    def __category(self):
        """Compute our test category from its directory location."""

        root_expr = "(Ravenscar/.*|Ada|C|SanityCheck)"
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

    def __init__(self, extradrivers="", extracargs="", category=None):

        # Step 1: Compute the list of drivers to exercise ...
        # ---------------------------------------------------

        # Probe all those from src/ plus those explicitely provided. If that
        # is empty, see if we have bodies aimed at being exercised by common
        # drivers up-tree. Abort if there's nothing to exercise at all

        self.all_drivers = []
        self.__expand_drivers("src/test_*.adb " + extradrivers)
        self.__expand_drivers("src/test_*.c")

        if len(self.all_drivers) == 0:
            self.__expand_shared_drivers()

        thistest.stop_if (
            len(self.all_drivers) == 0,
            FatalError ("Request to exercise empty test_set"))


        # Step 2: Determine a few test parameters common to all drivers
        # -------------------------------------------------------------

        # - test category:

        self.category = category if category else self.__category()

        # - Set of xcovlevel values to exercise:

        # If we have an explicit level query, use that. Fallback to defaults
        # otherwise. When there are multiple levels for a category, e.g. mcdc
        # variants, pick the first one only in qualification mode.

        default_xcovlevels_for = {
            "stmt":     ["stmt"],
            "decision": ["stmt+decision"],
            "mcdc":     ["stmt+uc_mcdc", "stmt+mcdc"]}

        if thistest.options.xcov_level:
            self.xcovlevels = [thistest.options.xcov_level]
        else:
            defaults = default_xcovlevels_for [self.category]
            self.xcovlevels = (
                [defaults[0]] if thistest.options.qualif_level else defaults)

        # - compilation arguments:

        self.cargs = to_list (extracargs)

        # Setup qualification data for this testcase

        self.qdata = Qdata(tcid=TEST_DIR)

    # ---------------------
    # -- run and helpers --
    # ---------------------

    def register_qde_for (self, drvo):
        """Register a qualif data entry for the just executed driver object"""
        self.qdata.register (QDentry(eid="blob", xrnotes=drvo.xrnotes))

    def run(self):

        try:

            # First, run the test for each driver, individually.
            [self.register_qde_for (
                    SCOV_helper(drivers=[driver],
                                xfile=driver, category=self.category,
                                xcovlevel=covlevel).run(self.cargs)
                    )
             for covlevel in self.xcovlevels
             for driver in self.all_drivers]

            # Next, run applicable consolidation tests.
            [self.register_qde_for (
                    SCOV_helper(drivers=self.__drivers_from(cspec),
                                xfile=cspec, category=self.category,
                                xcovlevel=covlevel).run(self.cargs)
                    )
             for covlevel in self.xcovlevels
             for cspec in ls ("src/cons_*.txt")]

        finally:

            # If we are running for qualification purposes, dump data needed
            # for qualification test-results production purposes. try/finally
            # is critical in making sure we dump results in case of failure
            # with exception as well.

            if thistest.options.qualif_level:
                self.qdata.flush()
