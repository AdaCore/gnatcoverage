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

class Category:
    def __init__(self, name, strength):
        self.name = name
        self.strength = strength

class CAT:
    auto = Category (
        name = "auto",
        strength = 1000
        )

# ===================================
# == Coverage Results User Control ==
# ===================================

# Facility to allow GPR level control. Still experimental.

class CovControl:

    def __init__ (
        self, units_in = None, ulist_in = None,
        units_out = None, ulist_out = None, xreports = None):

        # To control Units related attributes in the GPR Coverage package:

        self.units_in = units_in
        self.units_out = units_out
        self.ulist_in = ulist_in
        self.ulist_out = ulist_out

        # To instruct the testsuite driver about source reports we expect.
        # None means unspecified. Very different from specified empty.

        self.xreports = xreports

    def unexpected (self, source):
        return (
            self.xreports is not None
            and source not in self.xreports)

    def expected (self, source):
        return not self.unexpected (source)

    def __gprattrname (self, for_list, to_exclude):
        return "%(prefix)s%(kind)s" % {
            "prefix": "Excluded_" if to_exclude else "",
            "kind": "Units_List" if for_list else "Units"
            }

    def __gprattr (self, value, for_list, to_exclude):
        attrname = self.__gprattrname (for_list=for_list, to_exclude=to_exclude)
        return (
            ("for %s use \"%s\";" % (attrname, value)) if value and for_list
            else
            ("for %s use (%s);" % (
                    attrname, ','.join (['\"%s\"' % v for v in value])
                    )) if value and not for_list
            else
            ("-- empty %s" % attrname)
            )

    def gpr (self):
        return '\n'.join ([
                "package Coverage is",
                self.__gprattr (
                    for_list = False, to_exclude = False, value = self.units_in),
                self.__gprattr (
                    for_list = False, to_exclude = True, value = self.units_out),
                self.__gprattr (
                    for_list = True, to_exclude = False, value = self.ulist_in),
                self.__gprattr (
                    for_list = True, to_exclude = True, value = self.ulist_out),
                "end Coverage;"])

# ==============
# == TestCase ==
# ==============

class TestCase:

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
            for prefix in ("../" * n for n in range(1, 3)):
                if drivers: self.__expand_drivers (
                    "%(p)ssrc/test_%(b)s*.adb %(p)ssrc/test_%(b)s*.c"
                    % {'p' : prefix, 'b' : body}
                    )
                if cspecs: self.__expand_cspecs (
                    "%ssrc/cons_%s*.txt" % (prefix, body)
                    )

    def __category(self):
        """Compute our test category from its directory location."""

        for crit in ("stmt", "decision", "mcdc"):
            if re.search ("/%s(/|$)" % crit, TEST_DIR):
                return crit

        raise FatalError(
            "Unable to determine test category from dir '%s'" % TEST_DIR)

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

    def __init__(self, extradrivers="", extracargs="", category=CAT.auto):

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

        self.category = (
            self.__category() if category == CAT.auto else category)

        # - Set of xcovlevel values to exercise:

        # If we have an explicit level query, use that. Fallback to defaults
        # otherwise. When there are multiple levels for a category, e.g. mcdc
        # variants, pick the first one only in qualification mode.

        default_xcovlevels_for = {
            # A test without category should be ready for anything.
            # Exercise with the strictest possible mode.

            None: ["stmt+mcdc"],

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
        """Register a qualif data entry for driver object DRVO, about to
        be executed"""

        self.qdata.register (
            QDentry (xfile=drvo.xfile,
                     drivers=drvo.drivers, xrnotes=drvo.xrnotes)
            )
        return drvo

    def run(self, covcontrol = None):

        # We have exception processing below, which needs to operate
        # in the test root dir.

        homedir = os.getcwd()

        try:

            # First, run the test for each driver, individually.
            [self.register_qde_for (
                    SCOV_helper(drivers=[driver],
                                xfile=driver, category=self.category,
                                xcovlevel=covlevel, covcontrol=covcontrol)
                    ).run(self.cargs)
             for covlevel in self.xcovlevels
             for driver in self.all_drivers]

            # Next, run applicable consolidation tests.
            [self.register_qde_for (
                    SCOV_helper(drivers=self.__drivers_from(cspec),
                                xfile=cspec, category=self.category,
                                xcovlevel=covlevel, covcontrol=covcontrol)
                    ).run(self.cargs)
             for covlevel in self.xcovlevels
             for cspec in self.all_cspecs]

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
