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

import os

from SUITE.utils import frame, to_list, list_to_file, match, contents_of
from SUITE.utils import gprbuild, gprfor, SCOV_CARGS
from SUITE.utils import xrun, xcov

from gnatpython.fileutils import cd, mkdir

from notes import *
from expanders import *

# ======================================
# == SCOV_helper and internal helpers ==
# ======================================

# Dictionary of lists with default on read:

class ListDict(dict):
    def __init__(self):
        dict.__init__(self)

    def __getitem__(self, key):
        if not dict.has_key(self, key):
            dict.__setitem__(self, key, [])
        return dict.__getitem__(self, key)

# --------------
# -- Xchecker --
# --------------

class Xchecker:
    """Internal eXpectation marks checker class. This is used by SCOV_helper
    to compare sets of lines where coverage marks are expected with sets of
    lines where actual coverage marks were found in a report."""

    # --------------
    # -- __init__ --
    # --------------
    def __init__(self, report, xdict, edict):
        self.n_failed_init = thistest.n_failed
        self.xdict = xdict
        self.edict = edict
        self.report = report

    def register_failure(self, comment):
        thistest.failed("("+self.report+") " + comment)

    def try_sat_over(self, ekind, xn):

        # See if expected note XN is satisfied by one of the emitted notes of
        # kind EKIND. Store to sat dictionary accordingly. Note that we only
        # check for section inclusions here, so don't validate the correctness
        # of exemption justifications at this stage.

        for en in self.edict [ekind]:

            # Register a discharge and return as soon as segments match.
            # Complain about report section mismatches, nevertheless.

            if en.segment.within (xn.segment):
                en.discharges = xn
                xn.discharger = en
                self.sat[xn.block].append(xn)
                thistest.fail_if (
                    en.rsid != xn.rsid, "discharge section mismatch")
                return

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

        for xn in self.unsat[block]:
            dev_p = deviation_p(xn.kind)
            pos_p = positive_p(xn.kind)
            if (not xn.weak
                and ((not dev_p and not pos_p)
                     or (dev_p and not dsat_p)
                     or (pos_p and not psat_p))):
                self.register_failure (
                    "Expected %s mark missing at %s"
                    % (NK_image[xn.kind], str(xn.segment)))

    def register_unsat(self, xn):
        self.unsat[xn.block].append(xn)

    def process_xkind (self, xkind, ekinds):

        # Process expected notes of kind XKIND looking for candidate
        # dischargers in emitted noted of kinds EKINDS.

        xnotes = self.xdict[xkind]

        self.sat = ListDict()
        [self.try_sat_over(ekind, xn)
         for xn in xnotes for ekind in ekinds if not xn.discharger]

        self.unsat = ListDict()
        [self.register_unsat(xn) for xn in xnotes if not xn.discharger]

        [self.process_unsat(block) for block in self.unsat]

    def process_ekind(self, ekind):

        # Process emitted notes of kind EKIND, after we're done processing
        # all the relevant expected notes.

        if not strict_p(ekind): return

        enotes = self.edict[ekind]

        [self.register_failure(
                "Unexpected %s mark at %s" %
                (NK_image[en.kind], str(en.segment)))
         for en in enotes if not en.discharges]

    def process_notes(self, relevant_note_kinds, discharge_kdict):
        thistest.log ("\n~~ processing " + self.report + " ~~\n")

        # For each kind in RELEVANT_NOTE_KINDS, process discharges of
        # expectations from emitted notes. DISCHARGE_KDICT provides a special
        # set of of emitted note kinds that may discharge a given kind of
        # expected note, when that set is wider than the target kind alone.

        # We have to do this with two distinct loops because there is no
        # guaranteed one to one correspondance between emitted vs expected
        # kinds for discharges.

        # Process expected notes first, complaining about those that are not
        # discharged (expected bla missing).

        [self.process_xkind(
                xkind = xkind, ekinds = discharge_kdict.get (xkind, [xkind]))
         for xkind in relevant_note_kinds]

        # Then process emitted notes, complaining about those that don't
        # discharge any expectation as required (unexpected blo).

        [self.process_ekind(ekind) for ekind in relevant_note_kinds]

        # Dump the report contents in case this check exposed a test failure:

        if  thistest.n_failed > self.n_failed_init:
            thistest.log("\nreport contents:\n")
            thistest.log(contents_of(self.report))
        else:
            thistest.log("OK\n")

# -----------------
# -- SCOV_helper --
# -----------------

class SCOV_helper:
    """Helper class for source coverage activities."""

    # --------------
    # -- __init__ --
    # --------------
    def __init__(self, drivers, xfile, category, xcovlevel):
        self.drivers = [os.path.basename(os.path.splitext(d)[0])
                        for d in drivers]
        self.category = category
        self.xcovlevel = xcovlevel

        # Internal attributes: Directory where the instantiation takes place,
        # base prefix of Working Directory names, and original expectations
        # file.

        self.homedir = os.getcwd()+"/"
        self.wdbase  = "tmp_"
        self.xfile   = xfile

        # { sourcename -> KnoteDict } dictionaries of emitted/expected
        # line/report notes. We'll extract emitted notes from reports when we
        # know they have been produced. We extract expected notes from the
        # provided expectation file

        self.elnotes = {}
        self.ernotes = {}

        xnotes = XnotesExpander (xfile, xcovlevel)
        self.xlnotes = xnotes.xlnotes
        self.xrnotes = xnotes.xrnotes

    # -----------------
    # -- consolidate --
    # -----------------
    def consolidate(self):
        """Whether SELF instantiates a consolidation test."""
        return len(self.drivers) > 1

    # ----------------
    # -- singletest --
    # ----------------
    def singletest(self):
        """Whether SELF instantiates a single test."""
        return len(self.drivers) == 1

    # ----------------
    # -- locate_ali --
    # ----------------
    def locate_ali(self,source):
        """Return the fullpath of the ali file corresponding to the given
        SOURCE file.  Return None if none was found.
        """

        # Whatever the kind of test we are (single or consolidation), we
        # expect every ALI file of interest to be associated with at least
        # one single test, and to be present in the "obj" subdirectory of
        # the associated working dir.

        # Compute the local path from single test workdir and iterate over
        # working dir for all our drivers until we find. There might actually
        # be several instances in the consolidation case. We assume they are
        # all identical, and they should be for typical situations where the
        # same sources were exercised by multiple drivers:

        lali="obj/"+os.path.basename(os.path.splitext(source)[0] + ".ali")
        for main in self.drivers:
            tloc=self.awdir_for(main)+lali
            if os.path.exists(tloc):
                return tloc

        return None

    # --------------
    # -- ali_list --
    # --------------
    def ali_list(self):
        """Return a list of ali files corresponding to the list of sources
        specified in this tests's UXset.
        """
        all_alis = {}
        for ali in [self.locate_ali(source) for source in self.xrnotes]:
            if ali:
                all_alis[ali] = 1
        return all_alis.keys()

    # ---------
    # -- run --
    # ---------
    def run(self, extracargs=""):
        """Evaluate source coverage as exercised by self.drivers over
        the sources in self.UXset. Compare actual errors with expected
        specs in the associated LXset."""

        self.log()

        # Whatever the kind of test, we get to a Working Directory and
        # switch back when done:
        self.to_workdir(self.rwdir())

        # For single tests (no consolidation), we first need to build and
        # xcov run to get an execution trace:
        if self.singletest():
            self.build(self.drivers[0],extracargs)
            self.alis = list_to_file(self.ali_list(), "alis.list")
            self.xcov_run(self.drivers[0])
        else:
            self.alis = list_to_file(self.ali_list(), "alis.list")

        # Then, whatever the kind of test again, run xcov to get actual
        # coverage reports and check against our Xpectation specs.
        self.gen_xcov_reports()
        self.check_expectations()

        self.to_homedir()
        thistest.flush()

    # -------------------------
    # -- working directories --
    # -------------------------

    # Single tests (no consolidation) are each assigned a particular
    # working directory, named after their main unit with a prefix.

    # Consolidation tests are assigned a separate working directory as
    # well, and need access to each of the single directories to retrieve
    # execution traces and binaries.

    def rwdir_for(self,main):
        """Relative path to Working Directory for single MAIN."""
        return self.wdbase + main + "/"

    def awdir_for(self,main):
        """Absolute path to Working Directory for single MAIN."""
        return self.homedir + self.rwdir_for(main)

    def rwdir(self):
        """Relative path to Working Directory for current instance."""

        # For a single test, discriminate with driver basename. For a
        # consolidation test, discriminate with the expectation file basename.
        # We need the latter to allow multiple consolidation scenarii for a
        # testcase.

        if self.singletest():
            return self.rwdir_for(self.drivers[0])
        else:
            return self.rwdir_for(
                os.path.basename(os.path.splitext(self.xfile)[0]))

    def awdir(self):
        """Absolute path to Working Directory for current instance."""
        return self.homedir+self.rwdir()

    # -----------
    # -- build --
    # -----------
    def build(self,main,extracargs):
        """gprBuild binary for main program MAIN"""

        # Seek a few tentative source dirs, for typical locations of test
        # sources from a working directory, and typical locations of common
        # support sources.

        gprbuild(
            gprfor (mains = [main+".adb"], prjid="gen",
                    srcdirs = ["../"*n + "src" for n in range (1, 7)]),
            cargs = to_list(SCOV_CARGS) + ["-gnat05"] + to_list(extracargs))

    # --------------
    # -- xcov_run --
    # --------------
    def xcov_run(self,main):
        """run MAIN through "xcov run" to produce an execution trace."""

        # Feed xcov run with full path (absolute dir) of the program so we
        # can directly get to the binary from the trace when reading it from
        # a different directory, such as in consolidation tests.

        ofile="xcov_run_%s.out" % main
        xrun([self.awdir_for(main)+main,
              "--level=%s" % self.xcovlevel, "--scos=@%s" % self.alis],
             out=ofile)

        thistest.fail_if (
            match ("!!! EXCEPTION RAISED !!!", ofile),
            "exception raised while running '%s'." % main);

    # -------------------------
    # -- gen_one_xcov_report --
    # -------------------------
    def gen_one_xcov_report(self, traces, format, options=""):
        """Helper for gen_xcov_reports, to produce one specific report for a
        particulat FORMAT, from a provided list of TRACES over a provided list
        of ALIS. The command output is saved in a file named FORMAT.out."""

        p = xcov (args = ['coverage', '--scos=@'+self.alis,
                          '--level='+self.xcovlevel,
                          '--annotate='+format, "@"+traces] + to_list(options),
                  out = format+".out")

    # ----------------------
    # -- gen_xcov_reports --
    # ----------------------

    def force_xcov_report(self, filename):

        if not os.path.exists (filename):
            report = open(filename, 'w')
            report.write ("dummy xcov report")
            report.close

    def gen_xcov_reports(self):
        """Generate the reports against which we will check expectation
        specs. Request both the xcov format, which produces .ad?.xcov outputs,
        and the report format, which gets saved as report.out."""

        traces = list_to_file(
            [self.awdir_for(main)+main+'.trace' for main in self.drivers],
            "traces.list")

        self.gen_one_xcov_report(traces, format="xcov")

        # When nothing of possible interest shows up for a unit, xcov
        # generates nothing at all. Create dummy reports here to prevent
        # fatal exceptions trying to open them downstream.
        [self.force_xcov_report(source+'.xcov') for source in self.xrnotes]

        self.gen_one_xcov_report(
            traces, format="report", options="-o test.rep")

    # ------------------------
    # -- check_expectations --
    # ------------------------
    def check_expectations(self):

        # Expand the reports into source->emitted-notes dictionaries
        # and check against our per-source expectations.

        self.elnotes = LnotesExpander("*.xcov").elnotes
        self.ernotes = RnotesExpander("test.rep").ernotes

        [self.check_expectations_over (source) for source in self.xrnotes]

    def check_expectations_over(self, source):
        """Process expectations for a particular source, comparing
        expected coverage marks against what is found in the xcov reports
        for this source."""

        frame ("Processing UX for %s" % (source), post=0, char='~').display()

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

        # Symbolic strength of each category and context level, to let us
        # determine when we're running some test of a given catgeory with a
        # stricter --level

        strength = { "stmt": 1,          # category & context level
                     "decision" : 2,     # category
                     "mcdc" : 3,         # category
                     "stmt+decision": 2, # context
                     "stmt+mcdc": 3,     # context
                     "stmt+uc_mcdc": 3   # context
                     }

        stricter_level = strength [self.xcovlevel] > strength [self.category]

        # Line notes checks

        discharge_kdict = {
            # In stricter_level mode, we let ...

            # an emitted l! discharge an expected l+, when the l! is most
            # likely caused by irrelevant violations for the category
            lFullCov: [lFullCov, lPartCov],

            # an emitted lx1 discharge an lx0 expectation, when the extra
            # exempted violations are most likely caused by the level extra
            # strictness, hence irrelevant for the category
            lx0:      [lx0, lx1] } if stricter_level else {}

        Xchecker (
            source+'.xcov',
            self.xlnotes.get(source),
            self.elnotes.get(source, KnoteDict(lNoteKinds))
            ).process_notes(rp_lnotes_for[self.category], discharge_kdict)


        # Report notes checks

        discharge_kdict = {
            # In stricter_level mode, we let ...

            # an emitted xBlock1 discharge an xBlock0 expectation, as the
            # extra exempted violations are most likely irrelevant for the
            # category
            xBlock0: [xBlock0, xBlock1] } if stricter_level else {}

        Xchecker (
            'test.rep',
            self.xrnotes.get(source),
            self.ernotes.get(source, KnoteDict(rNoteKinds))
            ).process_notes(rp_rnotes_for[self.category], discharge_kdict)

    # ---------
    # -- log --
    # ---------
    def log(self):
        frame ("%s, %s\n%s coverage with --level=%s"
               % (str(self.drivers), self.xfile,
                  self.category, self.xcovlevel),
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

    # ----------------
    # -- to_homedir --
    # ----------------
    def to_homedir(self):
        """Switch to this test's homedir."""
        cd(self.homedir)

