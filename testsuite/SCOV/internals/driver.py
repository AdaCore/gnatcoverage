# ****************************************************************************
# **                         SCOV_helper abstraction                        **
# ****************************************************************************

# This modules offers the SCOV_helper class to allow writing things like:
#
#    t = SCOV_helper (
#          drivers="test_blob1.adb", xfile="test_blob1.adb",
#          category="stmt", xcovlevel="stmt")
#    t.run()

# This example is a request to exercise the single test_blob1 driver as a test
# of 'stmt' category, in a context where we'll call xcov with --level='stmt'
# to check that actual results match expectations described in test_blob1.adb
# itself.

# Consolidation tests are supported by passing multiple drivers and
# using an external text file to gather the expectations.

__all__ = ["SCOV_helper"]

# ****************************************************************************

from collections import defaultdict
import os

from SCOV.tctl import CAT, CovControl

from SCOV.instr import xcov_instrument

from SUITE.context import thistest
from SUITE.control import language_info
from SUITE.cutils import to_list, list_to_file, match, contents_of, no_ext
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprfor, cmdrun, xrun, xcov, frame
from SUITE.tutils import gprbuild_cargs_with
from SUITE.tutils import exename_for
from SUITE.tutils import srctracename_for, tracename_for, ckptname_for

from gnatpython.fileutils import cd, mkdir, ls

from . cnotes import r0, r0c, xBlock0, xBlock1, lx0, lx1, lFullCov, lPartCov
from . cnotes import KnoteDict, elNoteKinds, erNoteKinds, rAntiKinds
from . cnotes import xNoteKinds, sNoteKinds, dNoteKinds, cNoteKinds, tNoteKinds
from . cnotes import strict_p, deviation_p, anti_p, positive_p
from . cnotes import NK_image

from . xnexpanders import XnotesExpander
from . lnexpanders import LnotesExpander
from . rnexpanders import RnotesExpander

# =================
# == WdirControl ==
# =================

# Some testcases run for multiple categories in a row. We arrange for
# each run to execute in a different subdir to prevent mixups.
#
# Still, we sometimes want to reuse the binary programs and ALI files of one
# run for another, only re-executing gnatcov run and gnatcov coverage to
# produce traces and reports in a separate subdir for a different --level.
#
# The typical situation is testcases of the mcdc category running
# --level=stmt+mcdc first followed by --level=stmt+uc_mcdc.

# Essentially, each testcase associates with two main subdirectory families:
#
# * Working subdirectories, where it will execute programs to
#   produce traces, execute gnatcov coverage to produce reports, match
#   reports against expectations, etc.
#
# * Binary subdirectories, where the binary programs to execute and
#   their ALIs are to be found.
#
# In the aforementioned situations (mcdc/uc_mcdc), the Working subdir of one
# variant may be used as the Binary subdir of another one.

# This class is here to let TestCase objects tell our SCOV_Helper about the
# base prefixes to use for Working and Binary directories, with a possible
# extra piece to account for in both of them.

# These are prefixes only. Different subdirs are created for each individual
# driver that participates in a testcase.  To illustrate, consider a testcase
# of "mcdc" category with two drivers and a consolidation spec:
#
#    test_drv1.adb
#    test_drv2.adb
#    cons_drv12.txt
#
# Assume this will be running for --level=stmt+mcdc and --level=stmt+uc_mcdc.
# We might arrange to have:
#
# * for --level=stmt+mcdc, working dir base = "mc_", bin dir base = "mc_"
#   which will produce:
#
#        mc_drv1/  binary programs and ALIs for drv1
#                  traces and reports for drv1
#
#        mc_drv2/  binary programs and ALIs for drv2
#                  traces and reports for drv2
#
#        mc_drv12/ traces and reports for drv12, using
#                  binaries from mc_drv1 and mc_drv2
#
# * for --level=stmt+uc_mcdc, working dir base = "uc_", bin dir base = "mc_"
#   which will produce:
#
#        uc_drv1/  traces and reports for drv1,
#                  reusing binary programs and ALIs from mc_drv1
#
#        uc_drv2/  traces and reports for drv2,
#                  reusing binary programs and ALIs from mc_drv2
#
#        uc_drv12/ traces and reports for drv12, using
#                  binaries from mc_drv1 and mc_drv2
#
# We could also go for an alternate scheme where we build in a build only
# directory first, then point the runs for each criterion there. This is for
# the TestCase implementation to decide.

class WdirControl:

    def __init__ (self, wdbase, bdbase, subdirhint):

        # WDBASE is the base prefix to use for testcase Working directory.
        # BDBASE is the base prefix to use for testcase Binary directory.
        # Fallback to WDBASE when not provided.

        # REUSE_BIN indicates when we should reuse binaries from BDBASE, when
        # it was provided explicitly. Account for an extra SUBDIRHINT in both
        # bases.

        self.wdbase = wdbase + subdirhint

        self.reuse_bin = bdbase is not None
        self.bdbase = (bdbase + subdirhint) if self.reuse_bin else self.wdbase

# ======================================
# == SCOV_helper and internal helpers ==
# ======================================

# Relevant expectations and emitted Line and Report notes for each test
# CATEGORY:
# ---------------------------------------------------------------------

# The dictionary keys are test categories here. The values tell what kind of#
# expectations and emitted notes we care about for a given key. "care about"
# means "match emitted notes with expectations, ignore otherwise".

# This is used to prevent complaints about reported indications that could
# show up and not be stated as expected when a test is run for a stricter
# qualification context than its category (e.g. a stmt coverage test run
# for a level B or A qualification level).

# Line notes remain the same across levels, albeit with different meanings.

# Relevant Line expectations for each category are the same as the relevant
# emitted line notes.

r_eln_for = { # relevant emitted line notes
    CAT.stmt:     elNoteKinds,
    CAT.decision: elNoteKinds,
    CAT.mcdc:     elNoteKinds
    }

r_lxp_for = { # relevant line expectations
    CAT.stmt:     r_eln_for[CAT.stmt],
    CAT.decision: r_eln_for[CAT.decision],
    CAT.mcdc:     r_eln_for[CAT.mcdc]
    }

# The set of Report expectations we need to care about varies across
# levels. We need to ignore decision related messages in stmt coverage tests,
# for instance.

# We do care about exemptions at every level (and need to watch out for
# changes in the number of violations exempted when running a given test in
# different contexts (for different target levels).

# The relevant report expectations for each category are the same as the
# relevant emitted report notes augmented with anti-expectations

r_ern_for = { # relevant emitted report notes
    CAT.stmt:     tNoteKinds+xNoteKinds+sNoteKinds,
    CAT.decision: tNoteKinds+xNoteKinds+sNoteKinds+dNoteKinds,
    CAT.mcdc:     tNoteKinds+xNoteKinds+sNoteKinds+dNoteKinds+cNoteKinds
    }

r_rxp_for = { # relevant report expectations
    CAT.stmt:     r_ern_for[CAT.stmt]+rAntiKinds,
    CAT.decision: r_ern_for[CAT.decision]+rAntiKinds,
    CAT.mcdc:     r_ern_for[CAT.mcdc]+rAntiKinds,
    }

# --------------
# -- Xchecker --
# --------------

class _Xchecker:
    """Internal eXpectation marks checker class. This is used by SCOV_helper
    to compare sets of lines where coverage marks are expected with sets of
    lines where actual coverage marks were found in a report."""

    # --------------
    # -- __init__ --
    # --------------
    def __init__(self, report, xdict, rxp, edict, ren):

        # Our point is to assess what relevant expectations of XDICT are
        # satisfied from relevant emitted notes in EDICT. The relevance
        # criteria wasn't applied to the dictionary contents, we are doing
        # that from lists of relevant kinds as RXP for expectations and REN
        # for emitted notes.

        self.xdict = xdict
        self.rxp = rxp

        self.edict = edict
        self.ren = ren

        # We display the original report when this instance exposes test
        # failures. Remember what we need to be able to do that at the end.

        self.n_failed_init = thistest.n_failed
        self.report = report

    def register_failure(self, comment):
        thistest.failed("("+self.report+") " + comment)

    def __discharges (self, en, xn):
        """Whether emitted note EN discharges expected note XN, already
        known to be of the same kind."""

        # The emitted note needs to designate a sloc range within the
        # expected sloc range and separation tags, when any is expected,
        # must match.

        return (
            en.segment.within (xn.segment)
            and ((not xn.stag and not en.stag)
                 or (xn.stag and en.stag and en.stag.match (xn.stag)))
            )

    def try_sat_over(self, ekind, xn):

        # See if expected note XN is satisfied by one of the emitted notes of
        # kind EKIND which was not used to satisfy a prior expectation. Store
        # to sat dictionary accordingly.

        # Note that we only check for section inclusions here, so don't
        # validate the correctness of exemption justifications at this stage.

        # Ensuring that an emitted note is not used to satify multiple
        # expectations is stricter so the most correct in principle.

        for en in self.edict [ekind]:

            if not en.discharges and self.__discharges (en=en, xn=xn):
                en.discharges = xn
                xn.discharger = en
                self.sat[xn.block].append(xn)
                return

    def process_one_unsat (self, xn, some_dsat, some_psat):

        # Process one unsatisfied expected note XN, registering failure for
        # unsatisfied expectation unless we have reasons not to.
        # SOME_DSAT/PSAT tells if XN is within a fuzzy block where at least
        # one deviation/positive expectation was satisfied.

        # By definition, unsatisfied weak expectations are ok and report
        # anti-expectations are expected to be unmatched

        if xn.weak or anti_p(xn.kind):
            return

        # Then unmatched expectations are ok in fuzzy blocks where at least
        # one similar (positive or deviation) expectation was satisfied.

        if some_dsat and deviation_p(xn.kind):
            return

        if some_psat and positive_p(xn.kind):
            return

        self.register_failure (
            "Missing expected %s" % xn.image ()
            )

    def process_unsat(self, block):

        # Process unatisfied expected notes associated with BLOCK.
        # BLOCK is None for the set of expectations not part of a block.

        # Fuzzy matching: for deviations or positive notes, only raise failure
        # if none of the expected indications was satisfied (~OK if at least
        # one was emitted).

        # For each category, compute whether we have one expectation satisfied
        # in this fuzzy BLOCK:

        psat_p = False; dsat_p = False
        if block:
            for n in self.sat[block]:
                dsat_p |= deviation_p(n.kind)
                psat_p |= positive_p(n.kind)

        # Then complain about the unsatisfied stuff as necessary:

        [self.process_one_unsat (
                xn=xn, some_dsat=dsat_p, some_psat=psat_p)
         for xn in self.unsat[block]]

    def register_unsat(self, xn):
        self.unsat[xn.block].append(xn)

    def process_xkind (self, xkind, ekinds):

        # Process expected notes of kind XKIND looking for candidate
        # dischargers in emitted noted of kinds EKINDS.

        xnotes = self.xdict[xkind]

        self.sat = defaultdict(lambda: [])
        [self.try_sat_over(ekind, xn)
         for xn in xnotes for ekind in ekinds if not xn.discharger]

        self.unsat = defaultdict(lambda: [])
        [self.register_unsat(xn) for xn in xnotes if not xn.discharger]

        [self.process_unsat(block) for block in self.unsat]

    def process_ekind(self, ekind):

        # Process emitted notes of kind EKIND, after we're done processing
        # all the relevant expected notes.

        if not strict_p(ekind): return

        enotes = self.edict[ekind]

        [self.register_failure("Unexpected %s" % en.image())
         for en in enotes if (not en.discharges) or anti_p(en.discharges.kind)]

    def run (self, discharge_kdict):
        thistest.log ("\n~~ processing " + self.report + " ~~\n")

        # For each kind in RELEVANT_XNOTE_KINDS, process discharges of
        # expectations from emitted notes. DISCHARGE_KDICT provides a special
        # set of of emitted note kinds that may discharge a given kind of
        # expected note, when that set is wider than the target kind alone.

        # We have to do this with two distinct loops because there is no
        # guaranteed one to one correspondance between emitted vs expected
        # kinds for discharges.

        # Process expectations first, looking for candidate dischargers and
        # complaining about violation expectations that were not discharged
        # (expected bla missing). Discharges of anti-expectations are also
        # registered here, silently.  Complaints will come out of the emitted
        # notes processing below.

        [self.process_xkind(
                xkind = xkind, ekinds = discharge_kdict.get (xkind, [xkind]))
         for xkind in self.rxp]

        # Then process the relevant emitted notes, complaining about those
        # that don't discharge any expectation as required, or that discharge
        # an anti expectation (unexpected blo).

        [self.process_ekind(ekind) for ekind in self.ren]

        # Dump the report contents in case this check exposed a test failure:

        if thistest.n_failed > self.n_failed_init:
            thistest.log("\nreport contents:\n")
            thistest.log(contents_of(self.report))
        else:
            thistest.log("OK\n")

# -----------------
# -- SCOV_helper --
# -----------------

class SCOV_helper:
    """Helper class for source coverage activities."""

    # The differences between the different kinds of traces (binary or source)
    # are handled by specializing a few operations.

    def mode_build(self):
        """For a single test (not consolidation), build the program to run
        out of the test sources.
        """
        raise NotImplementedError

    def mode_execute(self, main):
        """Execute the program designated by MAIN, arranging to produce an
        execution trace. Return the name of a file containing the execution
        output.
        """
        raise NotImplementedError

    def mode_coverage_sco_options(self):
        """Return the gnatcov options to use to convey how SCOs should
        be retrieved at gnatcov coverage analysis time.
        """
        raise NotImplementedError

    def mode_gprdeps(self):
        """Return a list of mode specific project file dependencies that should
        be added to a single test project file, as needed at build or execution
        time.
        """
        raise NotImplementedError

    def mode_tracename_for(self, pgm):
        raise NotImplementedError

    # --------------
    # -- __init__ --
    # --------------
    def __init__(
        self, testcase, drivers, xfile,
        xcovlevel, covctl, wdctl
        ):

        # The TESTCASE object that delegates the hard work to us :-)
        self.testcase = testcase

        # The set of DRIVER sources that we are to exercise. We use this
        # as a precise approximation of a set of main subprogram or local
        # executable names so care about basenames only:
        self.drivers = [os.path.basename(d) for d in drivers]

        # The "--level" argument we ought to use on gnatcov command lines:
        self.xcovlevel = xcovlevel

        # The CovControl object that controls aspects of our coverage
        # testing operations (project file dependencies, units for which
        # we expect reports to be produced, ...)
        self.covctl = covctl

        # Internal attributes: Directory where the instantiation takes place,
        # original expectations file, and base prefix of Working Directory
        # names

        self.homedir = os.getcwd()+"/"
        self.xfile   = xfile

        # The WdirControl object telling about the Working and Binary
        # subdir prefixes we are to use:
        self.wdctl = wdctl

        # Compute the gnatcov coverage specific extra options that we'll have
        # to pass. We need these early for Xnote expansions.

        self.covoptions = ['--level='+self.xcovlevel]
        if self.covctl:
            self.covoptions += to_list(self.covctl.covoptions)

        # Compute the list of test launch options strings that we need for
        # expectation CTL lines.

        ctl_opts = ['--trace-mode=%s' % thistest.options.trace_mode]

        self.extracargs = to_list (self.testcase.extracargs)

        # { sourcename -> KnoteDict } dictionaries of emitted/expected
        # line/report notes. We'll extract emitted notes from reports when we
        # know they have been produced. We extract expected notes from the
        # provided expectation file.

        # This needs to be done now, to make sure that we can register this
        # driver object with maximum details for qualification results before
        # run() is called, hence early wrt possible exception occurrences.

        self.elnotes = {}
        self.ernotes = {}

        xnotes = XnotesExpander (
            xfile=xfile, xcov_level=xcovlevel,
            ctl_opts  = ctl_opts,
            ctl_cov   = self.covoptions,
            ctl_cargs = gprbuild_cargs_with (thiscargs=self.extracargs),
            ctl_tags  = thistest.options.tags,
            ctl_cons  = [thistest.options.consolidate]
            )
        self.xlnotes = xnotes.xlnotes
        self.xrnotes = xnotes.xrnotes

        # Even though we remember them here, we won't be looking at the
        # xlnotes if we're running for qualification.

        # Empty expectation sets here mean we have not a single source on
        # which anything will be checked. This can only be a mistake and would
        # just pass if we let the processing continue.

        thistest.fail_if (
            not self.xlnotes, "empty xlnotes from %s !!" % xfile)
        thistest.fail_if (
            not self.xrnotes, "empty xrnotes from %s !!" % xfile)

    def sources_of_interest(self):
        """List of sources for which we have expectations to match."""
        return self.xrnotes.keys()

    def units_of_interest(self):
        """Set of units for which we have expectations to match, based
        on the list of sources for which we have expectations and assuming
        standard  use of '-' in filenames for child units or subunits
        (foo-bar.ads for package Foo.Bar).
        """
        return {no_ext(os.path.basename(soi)).replace('-', '.')
                for soi in self.sources_of_interest()}

    def programs(self):
        """List of base binary file names for the test drivers we are
        given to exercise.
        """
        return [no_ext(main) for main in self.drivers]

    # --------------------------
    # -- xcov_translation_for --
    # --------------------------
    def xcov_translation_for(self, source):
        """How a SOURCE reference in expectations translates as the basename
        of an =xcov annotated source file."""

        return source.replace('/', '-')

    # ----------------------------
    # -- report_translation_for --
    # ----------------------------
    def report_translation_for(self, source):
        """How a SOURCE reference in expectations translates in slocs
        found in =report outputs."""

        return os.sep.join(source.split('/'))

    # ----------------
    # -- singletest --
    # ----------------
    def singletest(self):
        """Whether SELF instantiates a single test."""
        return len(self.drivers) == 1

    # ---------
    # -- run --
    # ---------
    def run(self):
        """Evaluate source coverage as exercised by self.drivers"""

        self.log()

        # Whatever the kind of test, we get to a Working Directory and
        # switch back when done:
        self.to_workdir(self.rwdir())

        # If we are requested to convey units of interest through a project
        # file and don't have a coverage control object to obey, build one to
        # convey the units of interest:

        if thistest.options.gprmode and not self.covctl:
            self.covctl = CovControl(units_in=self.units_of_interest())

        # Assess whether we should be using a project file to convey units of
        # interest, either requested from the command line or for specific
        # test purposes:

        self.gprmode = (
            thistest.options.gprmode
            or (self.covctl
                and self.covctl.requires_gpr()))

        # Compute our GPR now, which we will need for build of single tests
        # and/or analysis later on if in gprmode.  Turn inlining off for the
        # driver unit, so we exercise the functional code as separately
        # compiled, not as an inlined version of it in a non-representative
        # driver context.

        # Most of the tests with coverage control operate within
        # an extra subdir level
        this_depth = (
            thistest.depth + 1 if self.covctl else thistest.depth)

        self.gpr = gprfor (
            mains = self.drivers, prjid="gen",
            srcdirs = [
                "../"*n + "src" for n in range (1, this_depth)],
            exedir = self.abdir(),
            main_cargs = "-fno-inline",
            langs = ["Ada", "C"],
            deps = self.mode_gprdeps() + \
                    (self.covctl.deps if self.covctl else []),
            extra = self.covctl.gpr () if self.covctl else "")

        # For single tests (no consolidation), we first need to build, then
        # to execute to get an execution trace.  All these we already have for
        # consolidation tests, and there's actually no need to build if we
        # were provided a bin directory to reuse:

        if self.singletest() and not self.wdctl.reuse_bin:
            self.mode_build ()

        # Do gnatcov run now unless we're consolidating.  We'll just reuse
        # traces from previous executions in the latter case.

        if self.singletest():
            self.run_test(main=no_ext(self.drivers[0]))

        # At this point, we have everything we need for the analysis. Either
        # from the just done build+run in the single test case, or from
        # previous such sequences in the consolidation case.  Run gnatcov
        # coverage to get actual coverage reports and check against our
        # Xpectation specs.

        self.gen_xcov_reports()
        self.check_expectations()

        self.to_homedir()
        thistest.flush()

        # Let callers retrieve execution data at will
        return self

    # -------------------------
    # -- working directories --
    # -------------------------

    # Single tests (no consolidation) are each assigned a particular
    # working directory, named after their main unit with a prefix.

    # Consolidation tests are assigned a separate working directory as
    # well, and need access to each of the single directories to retrieve
    # execution traces and binaries.

    def rdir_for(self, base, main):
        """Relative path to Working or Binary Directory for single MAIN."""

        # Strip a possible "test_" prefix. This allows shortening pathnames
        # and the prefix is pointless in providing a unique temp dir.

        return base + main.replace ("test_", "", 1) + "/"

    def rwdir_for(self,main):
        """Relative path to Working Directory for single MAIN."""

        return self.rdir_for (base = self.wdctl.wdbase, main = main)

    def rbdir_for(self,main):
        """Relative path to Binary Directory for single MAIN."""

        return self.rdir_for (base = self.wdctl.bdbase, main = main)

    def adir_for(self, rdir):
        """Absolute path from relative dir."""
        return self.homedir + rdir

    def awdir_for(self,main):
        """Absolute path to Working Directory for single MAIN."""
        return self.adir_for (self.rwdir_for(main))

    def abdir_for(self,main):
        """Absolute path to Binary Directory for single MAIN."""
        return self.adir_for (self.rbdir_for(main))

    def main(self):

        # For a single test, discriminate with driver basename. For a
        # consolidation test, discriminate with the expectation file basename.
        # We need the latter to allow multiple consolidation scenarii for a
        # testcase.

        return (
            no_ext(self.drivers[0]) if self.singletest()
            else os.path.basename(no_ext(self.xfile)))

    def rwdir(self):
        """Relative path to Working Directory for current instance."""
        return self.rwdir_for(self.main())

    def awdir(self):
        """Absolute path to Working Directory for current instance."""
        return self.adir_for (self.rwdir())

    def rbdir(self):
        """Relative path to Working Directory for current instance."""
        return self.rbdir_for(self.main())

    def abdir(self):
        """Absolute path to Working Directory for current instance."""
        return self.adir_for (self.rbdir())

    # --------------
    # -- run_test --
    # --------------
    def run_test(self, main):
        """Execute the MAIN program to produce an execution trace, and
        trigger a failure if it raises an unhandled exception."""

        out_file = self.mode_execute(main=main)

        thistest.fail_if (
            match (
                "(!!! EXCEPTION RAISED !!!"
                "|raised [A-Z_]+ : [-._a-zA-Z]+:[0-9]+ \w+)",
                out_file),
            "exception raised while running '%s'." % main)

    # -------------------------
    # -- gen_one_xcov_report --
    # -------------------------
    def gen_one_xcov_report(self, inputs, format, options=""):
        """Helper for gen_xcov_reports, to produce one specific report for a
        particulat FORMAT, from provided INPUTS. The command output is saved
        in a file named FORMAT.out."""

        # Compute the set of arguments we are to pass to gnatcov coverage.

        # When project files are used, force report output in the current
        # directory where it would be without a project file, and which the
        # project file might arbitrarily redirect otherwise. Doing this
        # conditionally prevents the gratuitous addition of command line
        # options which might be absent from the tool qualified interface
        # descriptions.

        covargs = [
            '--annotate='+format, inputs
            ] + (self.covoptions + to_list(options))

        if self.gprmode:
            covargs.append ('--output-dir=.')

        # Run, latching standard output in a file so we can check contents on
        # return.

        ofile = format+".out"
        p = xcov (args = ['coverage'] + covargs, out = ofile)

        # Standard output might typically contain labeling warnings issued
        # by the static analysis phase, or error messages issued when a trace
        # indicates that some unlabeled edge was taken.  None of this should
        # happen so we simply fail as soon as the output file is not empty.
        # Note that we do this in qualification mode as well, even though what
        # we're looking at is not stricly part of the qualified interface.

        thistest.fail_if (
            os.path.getsize (ofile) > 0,
            "xcov standard output not empty (%s):\n--\n%s"  % (
                ofile, contents_of (ofile))
            )

    # ----------------------
    # -- gen_xcov_reports --
    # ----------------------

    def force_xcov_report(self, source):

        filename = self.xcov_translation_for(source)+'.xcov'

        if not os.path.exists (filename):
            report = open(filename, 'w')
            report.write ("dummy xcov report")
            report.close()

    def gen_xcov_reports(self):
        """Generate the reports against which we will check expectation
        specs. Request the report format, saved as test.rep, and the xcov
        format (.ad?.xcov outputs) if we're not in qualification mode"""

        # Determine what options we are going to provide as the
        # assessement's inputs.

        # For a single driver, we always rely on a trace as input and we
        # produce a checkpoint for possible future consolidation if the
        # current execution mode calls for it:

        checkpoints = thistest.options.consolidate == 'checkpoints'

        single_driver = no_ext(self.drivers[0]) if self.singletest() else None

        use_checkpoint_inputs = checkpoints and not single_driver

        # We request traces as input with "@inputs.list", where the file
        # contains the list of traces to use, derived from the set of drivers.
        # We request checkpoints as inputs with "--checkpoint=@inputs.list",
        # where the file contains the list of checkpoints to use, derived
        # from the set of drivers as well:

        (input_opt, input_fn) = \
            ("--checkpoint=", ckptname_for) if use_checkpoint_inputs \
            else ("", self.mode_tracename_for)

        inputs = "%s@%s" % (
            input_opt, list_to_file(
                [self.awdir_for(pgm) + input_fn(pgm)
                 for pgm in self.programs()],
                "inputs.list"))

        # Determine what command line options we'll pass to designate units of
        # interest and maybe produce a coverage checkpoint. We don't need and
        # don't want to pass SCO options when using checkpoints as inputs.

        sco_options = (
            [] if use_checkpoint_inputs else self.mode_coverage_sco_options())

        save_checkpoint_options = (
            ["--save-checkpoint=%s" % ckptname_for(single_driver)]
            if single_driver and checkpoints else [])

        # Now produce the --annotate=report format:

        self.gen_one_xcov_report(
            inputs, format="report",
            options=sco_options + save_checkpoint_options + ['-o', 'test.rep'])

        # Then an alternate .xcov output format, unless we are performing a
        # qualification run, for which that format isn't appropriate. No need
        # to regenerate a coverage checkpoint there - it would convey the same
        # as what the --annotate=report already produced if a checkpoint is
        # needed.

        if thistest.options.qualif_level:
            return

        self.gen_one_xcov_report(
            inputs, format="xcov", options=sco_options)

    # ------------------------------
    # -- check_unexpected_reports --
    # ------------------------------

    def check_unexpected_reports (self):

        """Check that we don't have unexpected reports or notes."""

        [thistest.fail_if (
                self.covctl.unexpected (s),
                "report note found for %s, not in expected list" % s)
         for s in self.ernotes]

        [thistest.fail_if (
                self.covctl.unexpected (s.rstrip (".xcov")),
                "%s report found, for source not in expected list" % s)
         for s in ls ("*.xcov")]

    # ------------------------
    # -- check_expectations --
    # ------------------------
    def check_expectations(self):

        # Complain about report notes or xcov reports for unexpected
        # sources, when the list happens to be specified. We need the
        # source->emitted report notes expanded for this purpose.

        # Checking that we do have the expected reports will be performed by
        # the regular coverage expectation assessments triggered below.

        self.ernotes = RnotesExpander("test.rep").ernotes

        if self.covctl and self.covctl.xreports != None:
            self.check_unexpected_reports ()

        # When nothing of possible interest shows up for a unit, xcov
        # generates nothing at all. Create dummy reports here to prevent
        # fatal exceptions trying to open them downstream.

        [self.force_xcov_report(source) for source in self.xrnotes]

        # Now expand the reports into source->emitted-notes dictionaries
        # and check against our per-source expectations.

        self.elnotes = LnotesExpander("*.xcov").elnotes

        # Compute a few things that we will need repeatedly over all the
        # sources with expectations to match

        # When we're running for a level stricter than the test category
        # (e.g. running a stmt test with --level=stmt+decision), we
        #
        # * Just ignore some emitted notes, simply irrelevant for the catgory
        #   (e.g. dT-, which doesn't change the statement coverage status of
        #   the outer statement). This is conveyed by the rp_?notes_for sets.
        #
        # * Accept that some emitted notes discharge expectations of some
        #   other kind as well. This is conveyed by the discharge_kdict values
        #   below.
        #
        # * This mechanism is also used to convey that all the relevant notes
        #   may "discharge" anti-expectations, which will cause a test failure
        #   when that happens.

        # Symbolic strength of each category and context level, to let us
        # determine when we're running some test of a given catgeory with a
        # stricter --level

        strength = {
            CAT.stmt: 1,       # categories
            CAT.decision: 2,
            CAT.mcdc: 3,

            "stmt": 1,         # context levels
            "stmt+decision": 2,
            "stmt+mcdc": 3,
            "stmt+uc_mcdc": 3
            }

        stricter_level = (
            self.testcase.category and
            strength [self.xcovlevel] > strength [self.testcase.category]
            )

        # For tests without a category, we will pick the relevant note
        # kinds from the strictest category possibly corresponding to the
        # xcov-level.

        strictest_cat_for = {
            "stmt": CAT.stmt,
            "stmt+decision" : CAT.decision,
            "stmt+mcdc": CAT.mcdc
            }

        relevance_cat = (
            self.testcase.category if self.testcase.category
            else strictest_cat_for[self.xcovlevel])

        # Setup our report and line discharging configurations (kinds of
        # emitted notes that are allowed to discharge other kinds of expected
        # notes), for =report and =xcov outputs.

        # =report outputs, stricter_level micro relaxations first:

        r_discharge_kdict = ({
            # let an emitted xBlock1 discharge an xBlock0 expectation, as an
            # extra exempted violations are most likely irrelevant for the
            # category
            xBlock0: [xBlock0, xBlock1]} if stricter_level else {}
        )

        # Then augment with what is allowed to hit "0" or "0c" expectation
        # statements:

        r_discharge_kdict.update (
            {r0 : r_ern_for[relevance_cat],
             r0c : r_ern_for[relevance_cat]
             })


        # =xcov outputs, stricter_level micro relaxations only:

        l_discharge_kdict = ({
            # an emitted l! discharge an expected l+, when the l! is most
            # likely caused by irrelevant violations for the category
            lFullCov: [lFullCov, lPartCov],

            # an emitted lx1 discharge an lx0 expectation, when the extra
            # exempted violations are most likely caused by the level extra
            # strictness, hence irrelevant for the category
            lx0:      [lx0, lx1] } if stricter_level else {}
        )

        # Now process source by source, skipping those for which no report
        # is expected when the list happens to be specified

        [self.check_expectations_over (
            source=source, relevance_cat=relevance_cat,
            r_discharge_kdict=r_discharge_kdict,
            l_discharge_kdict=l_discharge_kdict)
         for source in self.xrnotes if (
                not self.covctl or self.covctl.expected (source))
        ]

    def check_expectations_over(
        self, source, relevance_cat, r_discharge_kdict, l_discharge_kdict):
        """Process expectations for a particular SOURCE, comparing
        expected coverage marks against what is found in the xcov reports
        for this source."""

        frame ("Processing UX for %s" % (source), post=0, char='~').display()

        # Source names in expectations might still contain path indications
        # when they reach here, to indicate that the path components are
        # expected to be conveyed in the gnatcov results (slocs in =report
        # outputs and report file name for =xcov outputs).

        # Report notes checks

        strans = self.report_translation_for(source)
        _Xchecker (
            report ='test.rep',
            xdict  = self.xrnotes.get(source),
            rxp    = r_rxp_for[relevance_cat],
            edict  = self.ernotes.get(strans, KnoteDict(erNoteKinds)),
            ren    = r_ern_for[relevance_cat]
            ).run (r_discharge_kdict)

        # Line notes checks, meaningless if we're in qualification mode

        if thistest.options.qualif_level:
            return

        strans = self.xcov_translation_for(source)
        _Xchecker (
            report = strans+'.xcov',
            xdict  = self.xlnotes.get(source),
            rxp    = r_lxp_for[relevance_cat],
            edict  = self.elnotes.get(strans, KnoteDict(elNoteKinds)),
            ren    = r_eln_for[relevance_cat]
            ).run (l_discharge_kdict)

    # ---------
    # -- log --
    # ---------
    def log(self):
        frame ("%s/ %s, %s\n%s coverage with %s"
               % (os.path.relpath (os.getcwd(), thistest.homedir),
                  str([no_ext(main) for main in self.drivers]),
                  self.xfile,
                  self.testcase.category.name if self.testcase.category
                  else "generic",
                  ' '.join (self.covoptions)),
               char='*').display()

    # ----------------
    # -- to_workdir --
    # ----------------
    def to_workdir(self,wdir):
        """Switch to work directory WDIR, creating it if necessary. WDIR is
        expected to be either absolute or relative from the homedir."""

        self.to_homedir()
        mkdir(wdir)
        cd(wdir)

        thistest.log("Work directory: %s" % os.getcwd())

    # ----------------
    # -- to_homedir --
    # ----------------
    def to_homedir(self):
        """Switch to this test's homedir."""
        cd(self.homedir)


class SCOV_helper_bin_traces(SCOV_helper):
    """SCOV_helper specialization for the binary execution trace based mode."""

    # Outline of the binary trace based scheme:
    #
    # * Compilation of the sources produces SCOS in .ali files,
    #
    # * Execution of a program produces a binary trace file
    #
    # * Analysis by gnatcov coverage for a single program takes the trace and
    #   a description of the units of interest by way of a designation of the
    #   corresponding ALI files containing SCOs, with -P or --scos.
    #
    # * Consolidation is achieved by either
    #
    #   - Passing all the traces and the global SCOS of interest via -P
    #     or --scos to gnatcov coverage, or
    #
    #   - Combining coverage checkpoints produced for each program right
    #     after their execution to generate a trace.
    #
    # A key characteristic of this scheme is that units of interest are
    # conveyed through ALI files at analysis time, either when consolidating
    # from traces or when producing intermediate coverage checkpoints.

    def mode_build(self):
        gprbuild(self.gpr, extracargs=self.extracargs)

    def mode_execute(self, main):

        out_file = 'xrun_{}.out'.format(main)

        # Feed xcov run with full path (absolute dir) of the program so we
        # can directly get to the binary from the trace when reading it from
        # a different directory, such as in consolidation tests.
        main_path = self.abdir_for(main) + exename_for(main)

        # Some execution engines (e.g. valgrind) do not let us distinguish
        # executed program errors from engine errors. Because of them, we
        # ignore here any kind of execution error for tests expected to trigger
        # failures (such as harness tests), assuming that they will perform
        # further checks that are bound to fail if the execution doesn't
        # proceed as expected somehow (e.g. not producing a trace).
        xrun([main_path, "--level=%s" % self.xcovlevel] +
             self.mode_coverage_sco_options(),
             out=out_file, register_failure=not self.testcase.expect_failures)

        return out_file

    def mode_coverage_sco_options(self):
        # If we have a request for specific options, honor that.  Otherwise,
        # if we are requested to convey unit of interest through project file
        # attributes, use our build project file which has been amended for
        # that. Otherwise, fallback to --scos with a list of ALIs we compute
        # here:

        if self.covctl and self.covctl.gprsw:
            return self.covctl.gprsw.as_strings
        elif self.gprmode:
            return ["-P%s" % self.gpr]
        else:
            return ["--scos=@%s" % list_to_file(self.ali_list(), "alis.list")]

    def mode_gprdeps(self):
        return []

    def mode_tracename_for(self, pgm):
        return tracename_for(pgm)

    def locate_ali(self,source):
        """Return the fullpath of the ali file corresponding to the given
        SOURCE file.  Return None if none was found.
        """

        # Whatever the kind of test we are (single or consolidation), we
        # expect every ALI file of interest to be associated with at least
        # one single test, and to be present in the "obj" subdirectory of
        # the associated binary dir.

        # Compute the local path from single test bindir and iterate over
        # binary dir for all our drivers until we find. There might actually
        # be several instances in the consolidation case. We assume they are
        # all identical, and they should be for typical situations where the
        # same sources were exercised by multiple drivers:

        lang_info = language_info(source)
        lali="obj/"+lang_info.scofile_for(os.path.basename(source))
        for main in self.drivers:
            tloc=self.abdir_for(no_ext(main))+lali
            if os.path.exists(tloc):
                return tloc

        return None

    def ali_list(self):
        """Return a set of ali files corresponding to the list of sources
        specified in this tests's UXset.
        """

        # It is legitimate for some sources to not have an associated ali, for
        # example Ada separate sub-units compiled as part of their parent. We
        # just skip those and will fail matching expectations if the SCOs are
        # nowhere else.

        # We might also have expectations for different sources that map to
        # the same ali, as for example with the spec and body of the same
        # package.  We make our result a set to prevent duplicates and xcov
        # warnings later on.

        return set (
            [ali for ali in (self.locate_ali(source)
                             for source in self.sources_of_interest()) if ali]
            )


class SCOV_helper_src_traces(SCOV_helper):
    """SCOV_helper specialization for the source instrumentation mode."""

    # Outline of the source instrumentation based scheme:
    #
    # * Instrumentation produces instrumented sources and a so called
    #   "instrumentation checkpoint", holding SCOs and data required to
    #   decode source traces later on, similar to executable files for
    #   binary traces.
    #
    #   Units of interest must be conveyed at this stage, through project
    #   file attributes, as they control which units are instrumented.
    #
    # * Execution of the instrumented code produces a so called "source
    #   trace" file.
    #
    # * Analysis for a single program proceeds by providing gnatcov coverage
    #   with the source trace file and the corresponding instrumentation
    #   checkpoint.
    #
    # * Consolidation is achieved by either
    #
    #   - Providing the set or source traces and the corresponding checkpoints
    #     to gnatcov coverage, or
    #
    #   - Combining coverage checkpoints produced for each program right
    #     after their execution to generate a trace.
    #
    # The main differences with the binary trace based scheme are:
    #
    # * Units of interest are conveyed at instrumentation time (even prior
    #   to build) for the instrumentation scheme vs at analysis time for the
    #   binary trace case)
    #
    # * Instrumentation checkpoints must be provided to gnatcov coverage
    #   together with the source traces. Unlike executables for binary traces,
    #   instrumentation checkpoints are not necessarily visible at program
    #   execution time so can't be recorded in the traces.

    def mode_build(self):

        # We first need to instrument, with proper selection of the units of
        # interest. Expect we are to provide this through a project file as
        # we have no LI file at hand:
        assert self.gprmode

        # If we have a request for specific options, honor that. Otherwise,
        # use the already computed project file for this test:
        if self.covctl and self.covctl.gprsw:
            instrument_gprsw = self.covctl.gprsw
        else:
            instrument_gprsw = GPRswitches(root_project=self.gpr)

        xcov_instrument(
            covlevel=self.xcovlevel, checkpoint='instr.ckpt',
            gprsw=instrument_gprsw)

        # Now we can build, instructing gprbuild to fetch the instrumented
        # sources in their dedicated subdir:
        gprbuild(
            self.gpr, extracargs=self.extracargs,
            gargs='--src-subdirs=gnatcov-instr')

    def mode_execute(self, main):
        out_file = 'cmdrun_{}.out'.format(main)
        main_path = self.abdir_for(main) + exename_for(main)
        cmdrun([main_path], out=out_file,
               register_failure=not self.testcase.expect_failures)
        return out_file

    def mode_coverage_sco_options(self):

        # Units of interest are conveyed at instrumentation time
        # and the corresponding SCOs are held by the instrumentation
        # checkpoints.

        instr_checkpoints_opt = "--checkpoint=@%s" % list_to_file(
            [self.abdir_for(pgm) + "instr.ckpt"
             for pgm in self.programs()],
            "instr-checkpoints.list")

        return [instr_checkpoints_opt]

    def mode_tracename_for(self, pgm):
        return srctracename_for(pgm)

    def mode_gprdeps(self):
        return ["gnatcov_rts_full.gpr"]
