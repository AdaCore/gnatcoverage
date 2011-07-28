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

from SUITE.context import thistest
from SUITE.control import language_info
from SUITE.cutils import to_list, list_to_file, match, contents_of
from SUITE.tutils import gprbuild, gprfor, xrun, xcov, frame

from gnatpython.fileutils import cd, mkdir

from . cnotes import r0, xBlock0, xBlock1
from . cnotes import KnoteDict, elNoteKinds, erNoteKinds, rAntiKinds
from . cnotes import xNoteKinds, sNoteKinds, dNoteKinds, cNoteKinds, tNoteKinds
from . cnotes import strict_p, deviation_p, anti_p, positive_p
from . cnotes import NK_image

from . expanders import XnotesExpander, LnotesExpander, RnotesExpander

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

def no_ext(filename):
    """Return the filename with the extension stripped away.

    This is mostly a shortcut for using splitext(...)[0], in order
    to make the code that makes these conversions easier to read."""
    return os.path.splitext(filename)[0]

# Relevant expectations and emitted Line and Report notes for each test
# CATEGORY:
# ---------------------------------------------------------------------

# The dictionary keys are test categories here. The values tell what kind of
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
    "stmt":     elNoteKinds,
    "decision": elNoteKinds,
    "mcdc":     elNoteKinds
    }

r_lxp_for = { # relevant line expectations
    "stmt":     r_eln_for["stmt"],
    "decision": r_eln_for["decision"],
    "mcdc":     r_eln_for["mcdc"]
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
    "stmt":     tNoteKinds+xNoteKinds+sNoteKinds,
    "decision": tNoteKinds+xNoteKinds+sNoteKinds+dNoteKinds,
    "mcdc":     tNoteKinds+xNoteKinds+sNoteKinds+dNoteKinds+cNoteKinds
    }

r_rxp_for = { # relevant report expectations
    "stmt":     r_ern_for["stmt"]+rAntiKinds,
    "decision": r_ern_for["decision"]+rAntiKinds,
    "mcdc":     r_ern_for["mcdc"]+rAntiKinds,
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

    def try_sat_over(self, ekind, xn):

        # See if expected note XN is satisfied by one of the emitted notes of
        # kind EKIND. Store to sat dictionary accordingly. Note that we only
        # check for section inclusions here, so don't validate the correctness
        # of exemption justifications at this stage.

        for en in self.edict [ekind]:

            # Register a discharge and return as soon as segments match.

            if en.segment.within (xn.segment):
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
            "Expected %s mark missing at %s"
            % (NK_image[xn.kind], str(xn.segment)))

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

    # --------------
    # -- __init__ --
    # --------------
    def __init__(self, drivers, xfile, category, xcovlevel):
        self.drivers = [os.path.basename(d) for d in drivers]
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

        # Even though we remember them here, we won't be looking at the
        # xlnotes if we're running for qualification.

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

        lang_info = language_info(source)
        lali="obj/"+os.path.basename(no_ext(source) + lang_info.scos_ext)
        for main in self.drivers:
            tloc=self.awdir_for(no_ext(main))+lali
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
            self.xcov_run(no_ext(self.drivers[0]))
        else:
            self.alis = list_to_file(self.ali_list(), "alis.list")

        # Then, whatever the kind of test again, run xcov to get actual
        # coverage reports and check against our Xpectation specs.
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
            return self.rwdir_for(no_ext(self.drivers[0]))
        else:
            return self.rwdir_for(
                os.path.basename(no_ext(self.xfile)))

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

        # Turn inlining off for the driver unit, to make sure we exercise
        # the functional code as separately compiled, not an inlined version
        # of it in a non-representative driver context.

        gprbuild(
            gprfor (mains = [main], prjid="gen",
                    srcdirs = [
                    "../"*n + "src" for n in range (1, thistest.depth)],
                    main_cargs = "-fno-inline"),
            cargs=to_list(extracargs))

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
        specs. Request the report format, saved as test.rep, and the xcov
        format (.ad?.xcov outputs) if we're not in qualification mode"""

        traces = list_to_file(
            [self.awdir_for(no_ext(main))+no_ext(main)+'.trace'
             for main in self.drivers],
            "traces.list")

        self.gen_one_xcov_report(
            traces, format="report", options="-o test.rep")

        if not thistest.options.qualif_level:
            self.gen_one_xcov_report(traces, format="xcov")

        # When nothing of possible interest shows up for a unit, xcov
        # generates nothing at all. Create dummy reports here to prevent
        # fatal exceptions trying to open them downstream.
        [self.force_xcov_report(source+'.xcov') for source in self.xrnotes]

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
        #
        # * This mechanism is also used to convey that all the relevant notes
        #   may "discharge" anti-expectations, which will cause a test failure
        #   when that happens.

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

        # Report notes checks
        # -------------------

        # Setup our discharging configuration for stricter_level mode,
        # then augment with What is allowed to hit a "0" report expectation
        # statement:

        discharge_kdict = {
            # let an emitted xBlock1 discharge an xBlock0 expectation, as
            # an extra exempted violations are most likely irrelevant for the
            # category
            xBlock0: [xBlock0, xBlock1]
            } if stricter_level else {}

        discharge_kdict.update (
            {r0 : r_ern_for[self.category]})

        # Then do check:

        _Xchecker (
            report ='test.rep',
            xdict  = self.xrnotes.get(source),
            rxp    = r_rxp_for[self.category],
            edict  = self.ernotes.get(source, KnoteDict(erNoteKinds)),
            ren    = r_ern_for[self.category]
            ).run (discharge_kdict)

        # Line notes checks, meaningless if we're in qualification mode
        # -------------------------------------------------------------

        if thistest.options.qualif_level:
            return

        # Setup our discharging configuration for stricter_level mode

        discharge_kdict = {
            # In stricter_level mode, we let ...

            # an emitted l! discharge an expected l+, when the l! is most
            # likely caused by irrelevant violations for the category
            lFullCov: [lFullCov, lPartCov],

            # an emitted lx1 discharge an lx0 expectation, when the extra
            # exempted violations are most likely caused by the level extra
            # strictness, hence irrelevant for the category
            lx0:      [lx0, lx1] } if stricter_level else {}

        # Then do check:

        _Xchecker (
            report = source+'.xcov',
            xdict  = self.xlnotes.get(source),
            rxp    = r_lxp_for[self.category],
            edict  = self.elnotes.get(source, KnoteDict(elNoteKinds)),
            ren    = r_eln_for[self.category]
            ).run (discharge_kdict)

    # ---------
    # -- log --
    # ---------
    def log(self):
        frame ("%s, %s\n%s coverage with --level=%s"
               % (str([no_ext(main) for main in self.drivers]),
                  self.xfile, self.category, self.xcovlevel),
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

