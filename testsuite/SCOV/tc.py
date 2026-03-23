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

from collections.abc import Iterable

import os
import os.path
import re

from e3.fs import ls

from SCOV.internals.driver import (
    SCOV_helper,
    SCOV_helper_bin_traces,
    SCOV_helper_src_traces,
    SCOV_helper_rust,
    WdirControl,
)
from SCOV.tctl import CAT, _Category, CovControl
from SUITE.context import thistest
from SUITE.cutils import to_list, contents_of, FatalError, ext
from SUITE.qdata import Qdata, QDentry


class TestCase:
    # Helpers for __init__

    def __expand_drivers(self, patterns: str | Iterable[str]) -> None:
        """Add to the list of drivers to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""
        for p in to_list(patterns):
            self.all_drivers.extend(ls(p))

    def __expand_cspecs(self, patterns: str | Iterable[str]) -> None:
        """Add to the list of consolidation specs to exercize the set of files
        corresponding to every glob pattern in PATTERNS."""
        for p in to_list(patterns):
            self.all_cspecs.extend(ls(p))

    def __with_extensions(self, pattern: str) -> str:
        """Given a filename PATTERN string without a language extension,
        return a string listing PATTERN with all the possible language
        extensions we expect for drivers."""

        # We expect .adb for Ada bodies, .c for C sources, .cpp
        # for C++ sources and .rs for Rust sources.
        return " ".join(
            "%s%s" % (pattern, ext) for ext in [".adb", ".c", ".cpp", ".rs"]
        )

    def __expand_shared_controllers(self, drivers: bool, cspecs: bool) -> None:
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
            sxx = {""}

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

    def __category_from_dir(self) -> _Category:
        """Compute test category from directory location."""

        test_dir = thistest.reldir
        for cat in CAT.critcats:
            if re.search(r"/%s" % cat.name, test_dir):
                return cat

        raise FatalError(
            "Unable to infer test category from subdir '%s'" % test_dir
        )

    def __drivers_from(self, cspec: str) -> list[str]:
        """
        Compute the set of drivers that need to be combined for consolidation
        purposes, extracted from the consolidation spec in CSPEC.
        """

        # Extract the drivers regular expression to match from the
        # consolidation spec file, ...

        re_match = re.match("drivers=(.*)", contents_of(cspec))
        assert re_match is not None

        drv_expr = re_match.group(1)

        # ... then construct and return the retricted list of drivers that
        # match this expression
        return [drv for drv in self.all_drivers if re.search(drv_expr, drv)]

    def __init__(
        self,
        extradrivers: str = "",
        extracargs: list[str] | str = "",
        category: _Category | None = CAT.auto,
        tolerate_messages: str | None = None,
        tolerate_cov_messages: str | None = None,
        expect_non_zero_code: bool = False,
        assert_lvl: str | None = None,
        fun_call_lvl: bool = False,
        gexpr_lvl: bool = False,
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

        # Pass tolerate_cov_messages to gnatcov coverage invocations (only when
        # using source traces).
        self.tolerate_cov_messages = tolerate_cov_messages

        self._available_bdbase: str | None = None

        # Step 1: Compute the list of drivers and consolidation specs
        #         to exercise
        # ------------------------------------------------------------

        # Drivers first. Probe all those from src/ plus those explicitely
        # provided. If that is empty, see if we have bodies aimed at being
        # exercised by common drivers up-tree. Abort if there's nothing to
        # exercise at all
        self.all_drivers: list[str] = []
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
        self.all_cspecs: list[str] = []
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

    def __xcovlevels(self) -> list[str]:
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

    def __register_qde_for(self, drvo: SCOV_helper) -> None:
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

    def __wdbase_for(self, covlevel: str) -> str:
        """
        Compute a short base prefix for the working directory that will
        contain the output of coverage analysis for level covlevel.

        Uses the first letter of the highest level ('s' for "stmt" or 'u' for
        "stmt+uc_mcdc") and the full name of the assertion level if assertion
        coverage is enabled. If function and call coverage is needed, append
        "fc".
        """
        levels = covlevel.split("+")

        if len(levels) == 1:
            return "s_"

        wdbase = levels[1][0]

        if self.assert_lvl:
            wdbase += self.assert_lvl

        if self.fun_call_lvl:
            wdbase += "fc"

        return wdbase + "_"

    def __run_one_covlevel(
        self, covlevel: str, covcontrol: CovControl | None, subdirhint: str
    ) -> None:
        """
        Run this testcase individual drivers and consolidation tests with
        --level=COVLEVEL, using the provided COVCONTROL parameters and
        requesting SUBDIRHINT to be part of temp dir names.
        """

        rust_test = any(ext(drv) == ".rs" for drv in self.all_drivers)
        thistest.stop_if(
            rust_test
            and not all(ext(drv) == ".rs" for drv in self.all_drivers),
            FatalError("Rust drivers with other drivers is not yet supported"),
        )

        this_scov_helper: type[SCOV_helper] | None = None
        if rust_test:
            this_scov_helper = SCOV_helper_rust
        elif thistest.options.trace_mode == "bin":
            this_scov_helper = SCOV_helper_bin_traces
        elif thistest.options.trace_mode == "src":
            this_scov_helper = SCOV_helper_src_traces
        assert this_scov_helper is not None

        # Compute the Working directory base for this level, then run the test
        # for each indivdual driver.
        this_wdbase = self.__wdbase_for(covlevel)

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

    def run(
        self, covcontrol: CovControl | None = None, subdirhint: str = ""
    ) -> None:
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
