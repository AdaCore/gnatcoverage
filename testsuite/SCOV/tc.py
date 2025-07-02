"""TESTCASE abstraction

This module exposes the TestCase class, designed to automate all the
processing required to run a source coverage qualification testcase setup
where test.py was found.

A typical use, using default parameters, is TestCase_Here().run().

Roughly:

* The instance initialization locates the applicable test_ drivers, then

* run() exercises them all (for each: build, emulate, analyze, match reports
  vs expectations), and evaluates associated consolidation requests, if any.

The class facilities may be used for regression tests as well, not part of
the qualification database.

This module also exposes the "thistest" global, allowing calls to
thistest.result() straight after run(), without requiring a dedicated import on
the client side.
"""

import os
import os.path
import re

from e3.fs import ls

from SCOV.internals.driver import (
    SCOV_helper_bin_traces,
    SCOV_helper_src_traces,
)
from SCOV.internals.driver import WdirControl
from SCOV.tctl import CAT
from SUITE.context import thistest
from SUITE.cutils import to_list, contents_of, FatalError
from SUITE.qdata import Qdata, QDentry


class TestCase:
    # Helpers for __init__

    def __expand_drivers(self, patterns):
        """Add to the list of drivers to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""
        for p in to_list(patterns):
            self.all_drivers.extend(ls(p))

    def __expand_cspecs(self, patterns):
        """Add to the list of consolidation specs to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""
        for p in to_list(patterns):
            self.all_cspecs.extend(ls(p))

    def __with_extensions(self, pattern):
        """Given a filename PATTERN string without a language extension,
        return a string listing PATTERN with all the possible language
        extensions we expect for drivers."""

        # We expect .adb for Ada bodies, .c for C sources, .cpp
        # for C++ sources and .rs for Rust sources.
        return " ".join(
            "%s%s" % (pattern, ext) for ext in [".adb", ".c", ".cpp", ".rs"]
        )

    def __expand_shared_controllers(self, drivers, cspecs):
        """Search and expand possible shared drivers and/or consolidation
        specs uptree for our local functional units."""

        # Shared drivers would be <updir>/test_<xx>*.(adb|c) for some possible
        # updir and every <xx> such that there is a src/<xx>*.(adb|c).
        #
        # Likewise for consolidation specs, as <updir>/cons_<xx>*.txt
        #
        # Gather *set* of <xx> candidates first, then expand the associated
        # possible lists of drivers (each maybe empty). Beware not to include
        # child or sub units, as these dont mirror as such in the set of test
        # drivers.
        sxx = {
            srcmatch.group(1)
            for srcmatch in (
                re.match(r"([a-z_]*).*\.(adb|c)", os.path.basename(src))
                for src in ls("src/*")
            )
            if srcmatch
        }

        # If there is no candidate body in src/, arrange to run all the
        # drivers. This is useful for test groups on GPR variations for
        # example, where we typically want to run all the drivers and check
        # the analysis results against different sets of SCOS.
        if not sxx:
            sxx = [""]

        for body in sxx:
            for prefix in ("../" * n for n in range(1, thistest.depth)):
                if drivers:
                    self.__expand_drivers(
                        self.__with_extensions(
                            "%ssrc/test_%s*" % (prefix, body)
                        )
                    )
                if cspecs:
                    self.__expand_cspecs("%ssrc/cons_%s*.txt" % (prefix, body))

    def __category_from_dir(self):
        """Compute test category from directory location."""

        test_dir = thistest.reldir
        for cat in CAT.critcats:
            if re.search(r"/%s" % cat.name, test_dir):
                return cat

        raise FatalError(
            "Unable to infer test category from subdir '%s'" % test_dir
        )

    def __drivers_from(self, cspec):
        """
        Compute the set of drivers that need to be combined for consolidation
        purposes, extracted from the consolidation spec in CSPEC.
        """

        # Extract the drivers regular expression to match from the
        # consolidation spec file, ...
        drv_expr = re.match("drivers=(.*)", contents_of(cspec)).group(1)

        # ... then construct and return the retricted list of drivers that
        # match this expression
        return [drv for drv in self.all_drivers if re.search(drv_expr, drv)]

    def __init__(
        self,
        extradrivers="",
        extracargs="",
        category=CAT.auto,
        tolerate_messages=None,
        expect_non_zero_code=False,
        assert_lvl=None,
        fun_call_lvl=False,
        gexpr_lvl=False,
    ):
        # By default, these test cases expect no error from subprocesses (xrun,
        # xcov, etc.)
        self.expect_failures = False
        # Specifically expect the test binary (eventually run in a
        # cross environment) to return a non-zero exit code.
        self.expect_non_zero_code = expect_non_zero_code

        # Pass tolerate_messages to gnatcov instrument invocations (see the doc
        # for xcov_instrument).
        self.tolerate_messages = tolerate_messages

        # Step 1: Compute the list of drivers and consolidation specs
        #         to exercise
        # ------------------------------------------------------------

        # Drivers first. Probe all those from src/ plus those explicitely
        # provided. If that is empty, see if we have bodies aimed at being
        # exercised by common drivers up-tree. Abort if there's nothing to
        # exercise at all
        self.all_drivers = []
        self.__expand_drivers(
            self.__with_extensions("src/test_*") + " " + extradrivers
        )

        if not self.all_drivers:
            self.__expand_shared_controllers(drivers=True, cspecs=False)

        thistest.stop_if(
            not self.all_drivers,
            FatalError("Request to exercise empty test_set"),
        )

        # Seek consolidation specs, then. Similar scheme, local check first,
        # then seek shared entities
        self.all_cspecs = []
        self.__expand_cspecs("src/cons_*.txt")

        if not self.all_cspecs:
            self.__expand_shared_controllers(drivers=False, cspecs=True)

        # Step 2: Determine a few test parameters common to all drivers
        # -------------------------------------------------------------

        # - test category:
        #
        # If automatic detection from subdir was requested, do that.
        # Otherwise, use the provided argument, which might be None or a
        # criterion related value.
        self.category = (
            self.__category_from_dir() if category == CAT.auto else category
        )

        self.assert_lvl = assert_lvl
        self.fun_call_lvl = fun_call_lvl
        self.gexpr_lvl = gexpr_lvl

        # - extra compilation arguments, added to what --cargs was provided to
        #   the testsuite command line:
        self.extracargs = extracargs

        # Step 3: Setup qualification data for this testcase
        # --------------------------------------------------

        self.qdata = Qdata(tcid=thistest.reldir)

    # Helpers for run

    def __xcovlevels(self):
        """Compute the list of --level values to exercise."""

        # If we have an explicit level query, use that. Fallback to defaults
        # otherwise. When there are multiple levels for a category, e.g. mcdc
        # variants, pick the first one only in qualification mode.
        if thistest.options.xcov_level:
            return [thistest.options.xcov_level]

        assert not thistest.options.qualif_level

        default_xcovlevels_for = {
            # Tests without categories should be ready for anything.
            # Exercise with the strictest possible mode:
            None: ["stmt+mcdc"],
            CAT.stmt: ["stmt"],
            CAT.decision: ["stmt+decision"],
            CAT.mcdc: ["stmt+mcdc", "stmt+uc_mcdc"],
        }

        # Add a "+" before the name of the assertion coverage level in order
        # to append it at the end of the the other specified coverage levels
        # passed to gnatcov.
        alvl = ("+" + self.assert_lvl) if self.assert_lvl else ""

        # Append "+fun_call" to activate function and call coverage if needed
        fclvl = "+fun_call" if self.fun_call_lvl else ""

        # Append "+gexpr" to activate guarded expression coverage if needed
        glvl = "+gexpr" if self.gexpr_lvl else ""

        return [
            d + alvl + fclvl + glvl
            for d in default_xcovlevels_for[self.category]
        ]

    def __register_qde_for(self, drvo):
        """
        Register a qualif data entry for driver object DRVO, about to be
        executed.
        """
        self.qdata.register(
            QDentry(
                xfile=drvo.xfile,
                drivers=drvo.drivers,
                xrnotes=drvo.xrnotes,
                wdir=os.path.normpath(drvo.awdir()),
            )
        )

    def __run_one_covlevel(self, covlevel, covcontrol, subdirhint):
        """
        Run this testcase individual drivers and consolidation tests with
        --level=COVLEVEL, using the provided COVCONTROL parameters and
        requesting SUBDIRHINT to be part of temp dir names.
        """

        this_scov_helper = (
            SCOV_helper_bin_traces
            if thistest.options.trace_mode == "bin"
            else (
                SCOV_helper_src_traces
                if thistest.options.trace_mode == "src"
                else None
            )
        )

        # Compute the Working directory base for this level, then run the test
        # for each indivdual driver.
        this_wdbase = this_scov_helper.wdbase_for(self, covlevel)

        wdctl = WdirControl(
            wdbase=this_wdbase,
            bdbase=self._available_bdbase,
            subdirhint=subdirhint,
        )

        for driver in self.all_drivers:
            drvo = this_scov_helper(
                self,
                drivers=[driver],
                xfile=driver,
                xcovlevel=covlevel,
                covctl=covcontrol,
                wdctl=wdctl,
            )
            self.__register_qde_for(drvo)
            drvo.run()

        # Now we have a common binary dir prefix to reuse
        if not self._available_bdbase:
            self._available_bdbase = this_wdbase

        # Next, run applicable consolidation tests
        wdctl = WdirControl(
            wdbase=this_wdbase,
            bdbase=self._available_bdbase,
            subdirhint=subdirhint,
        )

        for cspec in self.all_cspecs:
            drvo = this_scov_helper(
                self,
                drivers=self.__drivers_from(cspec),
                xfile=cspec,
                xcovlevel=covlevel,
                covctl=covcontrol,
                wdctl=wdctl,
            )
            self.__register_qde_for(drvo)
            drvo.run()

    def run(self, covcontrol=None, subdirhint=""):
        """
        Execute this testcase, using coverage configuration parameters from
        COVCONTROL and SUBDIRHINT prepended to each driver id in temp subdir
        names.
        """

        # We have exception processing below, which needs to operate
        # in the test root dir.
        homedir = os.getcwd()

        try:
            # Run the set of drivers and consolidation tests for each
            # appropriate xcovlevel. Arrange to reuse the binary subdir of the
            # first run in subsequent ones. For single-level runs, having
            # the binaries together with the other artifacts is convenient.
            self._available_bdbase = None
            for covlevel in self.__xcovlevels():
                self.__run_one_covlevel(
                    covlevel=covlevel,
                    covcontrol=covcontrol,
                    subdirhint=subdirhint,
                )
        finally:
            # If we are running for qualification purposes, dump data needed
            # for qualification test-results production purposes. try/finally
            # is critical in making sure we dump results in case of failure
            # with exception as well.
            #
            # This must be done in the test root dir and an exception might
            # have left us anywhere, so ...
            os.chdir(homedir)

            if thistest.options.qualif_level:
                self.qdata.flush()
