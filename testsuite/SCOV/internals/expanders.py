# ****************************************************************************
# **                              NOTE EXPANDERS                            **
# ****************************************************************************

# Expose facilities to construct per-unit dictionaries of expected or reported
# coverage notes, extracted from text files (=xcov outputs, =report outputs, 
# or driver sources).

# The result dictionary keys are source names and values are KnoteDict
# objects (per kind dictionary of note instances)
#
# { [sourcename] -> { [note kind] -> [ Cnote, Cnote, ... ],
#                     ...
#                   },
#   ...
# }

# Below is a rough sketch of the entities and classes involved:
#
#   test_blob1.adb       blob1.adb
#   ...                      v
#   Expect.Patterns   >  XnotesExpander
#     (XnoteP)               v
#                 .xlnotes = { sourcename - KnoteDict(lNoteKinds) of Xnote }
#                 .xrnotes = { sourcename - KnoteDict(rNoteKinds) of Xnote }
#
# xcov --annotate=xcov   -> *.xcov
#                            v LnotesExpander
#                  .elnotes = { sourcename - KnoteDict(lNoteKinds) of Enote }
#
# xcov --annotate=report -> test.rep
#                            v RnotesExpander
#                  .ernotes = { sourcename - KnoteDict(rNoteKinds) of Enote }

# ****************************************************************************

import re, os

from gnatpython.fileutils import ls

from . cnotes import *
from . xnotep import *
from . tfiles import *
from . segments import *
from . stags import Stag_from

from SUITE.control import LANGINFO, language_info
from SUITE.cutils import Identifier
from SUITE.tutils import all_cargs_for_build

# --------------------
# -- LnotesExpander --
# --------------------

# Construct a { source -> KnoteDict } dictionary of emitted Line Notes
# from =xcov outputs in files corresponding to a provided DOTXCOV_PATTERN
# (*.xcov for example).

class LnotesExpander:

    NK_for = {'.': lNoCode, '+': lFullCov, '-': lNoCov, '!': lPartCov,
              '#': lx0, '*': lx1}

    def process_tline(self, tline):
        m = re.match('\s*([0-9]+) (.):', tline.text)
        if m: self.elnotes[self.source].register (
            Enote (kind = self.NK_for[m.group(2)],
                   segment = Line(int(m.group(1))),
                   source = self.source))

    def listing_to_enotes(self, dotxcov):
        self.source = dotxcov.rsplit ('.', 1)[0]
        self.elnotes[self.source] = KnoteDict(elNoteKinds)
        Tfile (filename=dotxcov, process=self.process_tline)

    def __init__(self, dotxcov_pattern):

        # xcov --annotate=xcov produces a set of .xcov annotated unit sources,
        # each featuring a synthetic note per line.

        self.elnotes = {}
        [self.listing_to_enotes (dotxcov) for dotxcov in ls (dotxcov_pattern)]

# --------------------
# -- RnotesExpander --
# --------------------

# Construct a { source -> KnoteDict } dictionary of emitted Line Notes
# from a provided =report outputs.

# Report section classes, to let us control when looking for indication
# patterns and check that each appears in the section where we expect it.

# Abstract diagnostic line, a-la "p.adb:8:4 statement not executed". This
# is a general Sloc followed by some diagnostic text.

class Rdiagline:
    def __init__ (self, sloc, diag):

        # SLOC object and DIAGnostic text

        self.sloc = sloc
        self.diag = diag

    # Regular expression to try-match a text line in order to
    # produce a valid object of this class

    # Expect something like:
    #
    #      "andthen.adb:10:33: statement not covered",
    #       -----------:-----: ---------------------
    #       source name:segmt: diagnostic text
        
    re = Sloc.re + " (?P<diag>.*)"

def Rdiagline_from (text):

    p = re.match (Rdiagline.re, text)
        
    return Rdiagline (
        sloc = Sloc_from_match (p),
        diag = p.group ("diag")
        ) if p else None

# Abstract Report section

class Rsection:
    def __init__(self, name, re_start):
        self.name = name
        self.re_start = re_start

        self.n_starts = 0
        self.n_ends = 0

    def starts_on(self, rline):
        if not re.search (self.re_start, rline): return False

        self.n_starts += 1
        return True

    # def ends_on(self, rline):

    def check(self):
        thistest.fail_if (
            self.n_starts != self.n_ends,
            "(%s report section): %d starts != %d ends" % (
                self.name, self.n_starts, self.n_ends)
            )

    def value(self, count):
        return (0 if count=="No" else int(count))

# eNote sections (violations, exempted regions, or other messages)

class Nsection (Rsection):

    def __init__(self, name, re_start):
        Rsection.__init__(self, name=name, re_start=re_start)

        self.enotes = []

    # def nkind_for(self, rline)

    def try_parse_enote(self, rline):

        dline = Rdiagline_from (rline)

        # If no match at all, punt.

        if not dline: return None

        # Otherwise, we construct the Enote object incrementally, as we need
        # to sort out the note kind and separation tag from the diagnostic
        # text

        enote = Enote (
            segment=dline.sloc.section, source=dline.sloc.filename,
            kind=None, stag=None
            ) 

        # Fetch and remove a possible separation tag from the diganostic
        # text. Removal is useful to facilitate matching of other parts, hence
        # attempted first.
        
        def __stag_replacement_for (m):
            enote.stag = Stag_from (m.group(1))  # side effect on caller here
            return ""

        this_diag = re.sub (
            pattern=" \(from (.*)\)", repl=__stag_replacement_for,
            string=dline.diag)

        # Then determine the note kind from the remaining contents

        enote.kind = self.nkind_for (this_diag)

        if enote.kind == None:
            thistest.failed (
                "(%s =report section) '%s' ?" % (self.name, rline.rstrip('\n'))
                )
            return None
        else:
            return enote

    def try_parse(self, rline):
        enote = self.try_parse_enote(rline)
        if enote:
            self.enotes.append(enote)
        return enote

    def validate_ecount(self, count):
        self.ecount = len(self.enotes)
        thistest.fail_if (
            count != self.ecount,
            "(%s report section) recognized %d notes != summary (%d)\n" %
            (self.name, self.ecount, count))

        self.n_ends += 1
        return True

# Violations section

class Vsection (Nsection):
    def __init__(self, name, re_start, re_notes):
        Nsection.__init__(self, name=name, re_start=re_start)
        self.re_notes = re_notes

    def nkind_for(self, rline):
        for key in self.re_notes:
            if rline.find (key) != -1:
                return self.re_notes [key]
        return None

    def ends_on(self, rline):
        p = re.match ("(No|\d+) violation[s]*\.$", rline)
        return p and self.validate_ecount (count=self.value(p.group(1)))

# Other errors section

class Osection (Nsection):
    def __init__(self, name, re_start):
        Nsection.__init__(self, name=name, re_start=re_start)

    def nkind_for(self, rline):

        # Messages in this section are always unexpected and should trigger
        # test failure. Just tell we don't know how to bind them on any sort
        # of expected note kind, and let the generic engine do the rest.

        return None

    def ends_on(self, rline):
        p = re.match ("(No|\d+) message[s]*\.$", rline)
        return p and self.validate_ecount (count=self.value(p.group(1)))

# eXemptions section

class Xsection (Nsection):
    def __init__(self, name, re_start):
        Nsection.__init__(self, name=name, re_start=re_start)

    def nkind_for(self, rline):
        r = re.search ("(\d+) exempted violation", rline)
        return (None if not r
                else xBlock0 if int(r.group(1)) == 0 else xBlock1)

    def ends_on(self, rline):
        p = re.match ("(No|\d+) exempted region[s]*\.$", rline)
        return p and self.validate_ecount (count=self.value(p.group(1)))


# Analysis summary section

class Asection (Rsection):
    def __init__(self, name, re_start, skeys):
        Rsection.__init__(self, name=name, re_start=re_start)
        self.skeys = skeys
        self.checked = dict (
            [(sec, False) for sec in skeys])

    def try_match(self, sec, rline):
        p = re.match (self.skeys[sec], rline)

        if p:
            sum_count = self.value (p.group(1))
            sec_count = sec.ecount
            thistest.fail_if (
                sum_count != sec_count,
                "summary count %d != section count %d for %s" % (
                    sum_count, sec_count, sec.name)
                )
            thistest.fail_if (
                sec.n_starts != 1,
                "summary found for section starts != 1 (%s)" % sec.name
                )
            self.checked[sec] = True

    def try_parse(self, rline):
        [self.try_match(sec, rline)
         for sec in self.skeys if not self.checked[sec]]
        return None

    def ends_on(self, rline):
        p = re.match (".. END OF REPORT ..$", rline)
        if p:
            self.n_ends += 1
        return p

    def check (self):
        Rsection.check (self)

        [thistest.fail_if (
                sec.n_starts > 0 and not self.checked[sec],
                "summary count check missing for section %s" % sec.name
                ) for sec in self.skeys
         ]

# Set of sections in a report

class RsectionSet:
    def __init__(self):
        self.Xr = Xsection (
            name="XR", re_start="EXEMPTED REGIONS"
            )

        self.Sc = Vsection (
            name="STMT", re_start="STMT COVERAGE",
            re_notes = {
                "statement not executed": sNoCov,
                "multiple statements on line": sPartCov}
            )

        self.Dc = Vsection (
            name="DECISION", re_start="DECISION COVERAGE",
            re_notes = {
                "decision outcome FALSE never": dfNoCov,
                "decision outcome TRUE never": dtNoCov,
                "decision never evaluated": dNoCov,
                "decision not exercised in both directions": dPartCov}
            )

        mcdc_notes =  {
            "decision outcome FALSE never": efNoCov,
            "decision outcome TRUE never": etNoCov,
            "decision never evaluated": eNoCov,
            "decision not exercised in both directions": ePartCov,
            "condition has no independent influence pair": cPartCov
            }

        self.Uc = Vsection (
            name="UC_MCDC", re_start="UC_MCDC COVERAGE",
            re_notes = mcdc_notes
            )

        self.Mc = Vsection (
            name="MCDC", re_start=" MCDC COVERAGE",
            re_notes = mcdc_notes
            )

        self.Oe = Osection (
            name="OE", re_start="OTHER ERRORS"
            )

        self.vsections = (self.Sc, self.Dc, self.Uc, self.Mc)

        self.nsections = self.vsections + (self.Xr, self.Oe)

        self.As = Asection (
            name="AS", re_start="ANALYSIS SUMMARY",
            skeys = dict (
                [(s, "(No|\d+).* %s violation[s]*\.$" % s.name)
                 for s in self.vsections]
                + [(self.Xr, "(No|\d+) exempted region[s]*\.$")])
            )

        self.allsections = self.nsections + (self.As,)

    def starts_with (self, rline):
        for rs in self.allsections:
            if rs.starts_on (rline):
                return rs
        return None

    def check (self):
        [rs.check() for rs in self.allsections]

class RnotesExpander:
    """Produce list of Enote instances found in a "report" output."""

    def to_enotes(self, report):

        # We need to ignore everything not in the report sections
        # of interest, so until we know we're in ...

        self.rset = RsectionSet()
        self.rs = None

        self.report = report
        Tfile (filename=self.report, process=self.process_tline)

        self.rset.check()

    def register(self, enote):
        source = enote.source
        if source not in self.ernotes:
            self.ernotes[source] = KnoteDict(erNoteKinds)
        self.ernotes[source].register (enote)

    def process_tline(self, tline):

        rline = tline.text

        # Check if we are getting in a section of interest. If so, register
        # that and get to next line.

        rs = self.rset.starts_with (rline)
        if rs:
            self.rs = rs
            return None

        # Check if we are getting out of the current section of interest ...

        if self.rs and self.rs.ends_on(rline):
            self.rs = None

        # Skip this line if we're out of any section of interest

        if self.rs == None: return None

        enote = self.rs.try_parse(rline)

        # Some sections produce enotes, some don't (e.g. analysis summary).
        # An error is issued by the section processing if it should find one
        # but couldn't.

        if enote:
            self.register (enote)

        return enote

    def __init__(self, report):

        # xcov --annotate=report produces a single report featuring a list of
        # indications for slocs in all the units.

        self.ernotes = {}
        self.to_enotes (report)

# --------------------
# -- XnotesExpander --
# --------------------

# Construct two { source -> KnoteDict } dictionaries of expected coverage
# notes, one for line notes and one for report notes as expressed by user
# expectations found in a provided XFILE.

# We refer to the expressed user expectations as SCOV data, and parse it
# according to the following grammar:

#     SCOV.data := ucxgroup__list
#     ucxgroup__list := ucx_group <newline> [ucxgroup__list]
#     ucx_group := sources <new_line> lx_or_ctl__list

#     sources := "--# " filename_lists
#     filename_lists := filename_list ["|" filename_lists]
#     filename_list := FILENAME [ filename_list]

#     lx_or_ctl__list := lx_or_ctl <newline> [lx_or_ctl__list]
#     lx_or_ctl := lx | ctl

#     ctl := "-- " arg_ctl__list
#     arg_ctl__list := arg_ctl ["## " arg_ctl__list]
#     arg_ctl := "-- " ["!"]"%"<cargs|cov>":" optgroup__list
#     optgroup__list := optgroup ["," optgroup__list]
#     optgroup := <atomic option sequence like "-gnatp" or "-S routines">

#     lx := "-- " lx_lre lx_lnote_list " ## " lx_rnote_list <newline>
#     lx_lre := "/" REGEXP "/"
#     weak_mark := ~

#     cov_level_choice := <s|d|m|u>
#     cov_level_list := cov_level_choice [cov_level_list]
#     cov_level_test := cov_level_list "=>"

#     lx_lnote_list := lx_lnote_choice [";" lx_lnote_list]
#     lx_lnote_choice := [cov_level_test] [weak_mark] lx_lnote
#     lx_lnote := <l-|l!|l+|l*|l#|l0>

#     lx_rnote_list := lx_rnote_choice [lx_rnote_list]
#     lx_rnote_choice := [cov_level_test] [weak_mark] lx_rnote
#     lx_rnote_kind = <s-|s!|dT-|dF-|d!|eT-|eF-|oT-|oF-|c!|x0|x+>
#     lx_rnote := lx_rnote_kind[:"TEXT"][@(STAG)]

# The start of the SCOV data is identified as the first comment whose syntax
# matches a "sources" line.  Any comment before then is assumed to be a normal
# free-text comment.  Any comment thereafter is assumed to be part of the
# SCOV data.

# The "lx_lre" is a regular expression that is used to identify source lines
# that match "-- # " + lx_lre.  We decided to make the "-- # " implicit in
# order to reduce the lx_lre expression to a minimum as well as to force a
# standard format for all such markers.

# slashes inside lx_lre tokens are allowed. The SCOV_data parser simply
# uses the first and last slash as the delimiters.

# SEPARATION TAGS (STAG addends)
# ------------------------------
# An STAG addend, when specified, is the separation tag expected for the
# note. This will be a bare routine name for outputs with -S routines, and a
# nest of instantiation slocs for outputs with -S instances. In the latter
# case, slocs are stated in a symbolic manner, as "i:NAME" to denote a generic
# instantiation somewhere on a source line featuring an "i:NAME" anchor, for
# any source part of any of the groups.
#
# In absence of STAG part, the corresponding emitted notes must not contain
# any separation tag indication to match. A bare "s-" is only matched by a
# bare "statement not executed" =report indication for example, without any
# separation tag mentioned.

# CONDITIONAL EXPECTATIONS WITHIN A GROUP (CTL lines)
# ---------------------------------------------------
# %cargs: opt1[, opt2, ... optn] means: from now on, only grab the next-coming
# lx lines if opt1 (and opt2 and ... up to optn) are part of the compilation
# options for the test. A '!' at the beginning of an option inverts the logic
# for this option in particular, meaning "... if this option is *not* part of
# of the compilation args for the test.
#
# %cov: ... is similar, against the gnatcov coverage specific options for the
# test instead of the compilation flags.
#
# When a lx line is grabbed, it replaces whatever was previously recorded for
# the selecting regexp (lre) it holds, if anything.
#
# Here is a simple example:
#
# --# p.adb                         <= Start expectation group wrt p.adb
# --  /bla/ ...
# --
# --  /blu/ l! ## s-                <= By default (about to be conditionally
#                                      overriden), expect no separation tag
#                                      on s- for /blu/.
# 
# --%cov: -S instances              <= From now on, grab expectations only if
#                                      gnatcov coverage -S instances, then ...
# --  /blu/ l! ## s-@(i:myinst)     <= Override expectations for /blu/
# 
# --%cov: -S routines               <= Likewise for -S routines
# --  /blu/ l! ## s-@(myinst__blu)
#
# Conditioning on cargs works the same. Both kinds of controls may be
# and-combined with ## and each individual control may be inverted with
# a '!' prefix. For example:
#
# -- From now on, grab expectations only if gnatcov coverage -S routines
# -- and compilation without -gnatn:
#
# --%cov: -S routines ## %cargs: !-gnatn
#
# Note that contrary to STAGS where absence in expectation means we expect no
# emitted STAG at all, absence of an option in a CTL list means that it
# doesn't matter whether this option is actually passed or not. '!' is there
# for cases where we need to state dependency on option absence.

# We use three intermediate abstractions to build the dictionaries from
# the expectations text:
#
# * Line Coverage eXpectations (LineCX) objects, to represent individual
#   expectations line like  "--  /bla/ l- ## s-", and
#
# * Unit Coverage eXpecations (UnitCX) objects to represent the associations
#   of a sequence of line expectations with a single unit name.
#
# * Unit eXpectation Groups (UXgroup) objects to represent the associations of
#   a sequence of line expectations with a list of unit names.
#
# For example, assuming an Ada driver, the excerpt below ...
#
# --# p1.adb p2.adb
# --  /bla/ l- ## s-
# --  /blo/ l+ ## 0
#
# --# x.adb
# --  /blu/ l+ ## 0
#
# Will first yield:
#
# * One UCX group for [p1.adb, p2.adb] associated with
#   two LineCX objects, one for /bla/ and one for /blo/
#
# * One UCX group for [x.adb] associated with one LineCX
#   object for /blu/
#
# From there, we'll eventually produce three UnitCX objects:
#
# * One instantating internal note objects for /bla/ + /blo/ from p1.adb
#
# * One instantating internal note objects for /bla/ + /blo/ from p2.adb
#
# * One instantating internal note objects for /blu/ from x.adb

# ------------
# -- LineCX --
# ------------

class LineCX:
    """Line Coverage eXpectations class.  Associated with source file names,
    these define unit coverage expectation specs."""

    def __init__(self, lre, lnp, rnps, override):
        
        self.lre = lre
        self.lnp = lnp
        self.rnps = rnps

        # Whether this line is meant to override an existing one
        # with an identical lre:

        self.override = override

    def instanciate_lnotes_over(self, tline, block, srules):
        return [self.lnp.instanciate_over (tline, block, srules)]

    def instanciate_rnotes_over(self, tline, block, srules):
        return [rnp.instanciate_over (tline, block, srules)
                for rnp in self.rnps if rnp.kind]

# ------------
# -- UnitCX --
# ------------

class UnitCX:
    """Associate a single source name with a list of expected Coverage
    Line eXpectations. Construct the corresponding Line and Report Xnote
    dictionaries."""

    # expected notes instanciations
    # -----------------------------

    def instanciate_notes_for(self, lx, tline, block, srules):
        [self.xldict.register (ln)
         for ln in lx.instanciate_lnotes_over (tline, block, srules)]
        [self.xrdict.register (rn)
         for rn in lx.instanciate_rnotes_over (tline, block, srules) if rn]

    # fuzz block processing
    # ---------------------

    # We identify block with the help of explicit comments, not with lone
    # language constructs such as begin/end in Ada. Finding the proper couples
    # of the latter is not easy and error prone.

    def blopen_p (self, tline):
        return re.match ("^\s*begin\s*-- #", tline.text)

    def blclose_p (self, tline):
        return re.match ("^\s*end;\s*-- #", tline.text)

    def check_block_on (self, tline):

        if self.blopen_p(tline):
            self.current_block = Block (parent = self.current_block)

        if self.blclose_p(tline):
            thistest.stop_if (
                not self.current_block,
                FatalError ("end of nonexistant block at\n=> " + tline.text))
            self.current_block = self.current_block.parent

    # kind subsitution rules processing
    # ---------------------------------

    # For shared drivers that exercise boolean expressions in different
    # possible contexts (control constructs or others), determined by the
    # functional source. For example,
    #
    #    Assert (Andthen (A => True, B => True));
    #
    # doesn't exercise the False outcome of an and-then expression, always.
    # This could result in different output expectations depending on the
    # context where the expression is used.
    #
    # A shared driver would express this with "oF-", that needs to be turned
    # into, say, dF- or eF- according to hints in the functional source.  Such
    # hints are provided as :<subst-key>: at the end of line anchors, with the
    # following possible values for <subst-key> :

    subst_tuples_for = {

        # outcome expectations for line are to produce "decision"
        # expectations

        "o/d": {otNoCov : dtNoCov,
                ofNoCov : dfNoCov,
                oPartCov: dPartCov,
                oNoCov  : dNoCov},

        # outcome expectations for line are to produce "expression"
        # expectations

        "o/e": {otNoCov : etNoCov,
                ofNoCov : efNoCov,
                oPartCov: ePartCov,
                oNoCov  : eNoCov},

        # outcome expectations for line are to be ignored

        "o/0": {otNoCov : r0,
                ofNoCov : r0,
                oPartCov: r0,
                oNoCov  : r0,
                lPartCov: lFullCov
                },

        # eval on the line are in expression or decision context

        "e": {},
        "d": {}
        }

    def check_srules_on (self, tline):

        # Check for kind substitution rules on this line. Reset
        # at every line for now.

        m = re.search ("# .*:(.*):$", tline.text)

        if not m:
            self.current_srules = None
        else:
            self.current_srules = {}
            [self.current_srules.update (self.subst_tuples_for [sim])
             for sim in m.group(1).split(',')]

    # toplevel processing
    # -------------------

    def process_tline(self, tline):
        self.check_srules_on (tline)
        [self.instanciate_notes_for (
                lx, tline, self.current_block, self.current_srules)
         for lx in self.LXset if re.search (lx.lre, tline.text)]
        self.check_block_on (tline)

    def __init__(self, sourcepath, LXset):

        self.LXset = LXset

        # dictionaries of expected line and report notes for our unit

        self.xldict = KnoteDict(xlNoteKinds)
        self.xrdict = KnoteDict(xrNoteKinds)

        self.current_block = None
        self.current_srules = {}

        self.tfile  = Tfile (
            filename=sourcepath, process=self.process_tline)

        # Source names in expectations might contain paths, which facilitates
        # tests of GPR facilities with a project hierarchy.
        #
        # Record the source basename as our source attribute, which is used to
        # key in the various dictionaries and match the the name of annotated
        # source reports, always produced in the current directory only.

        self.source = os.path.basename (sourcepath)

        thistest.stop_if (
            self.current_block, FatalError ("fuzz block still open at EOF"))

# --------------
# -- UXgroup --
# --------------

class UXgroup:

    def __init__ (self, candlists):

        # SPLIST: good list of source paths from the set of candidate lists
        # received in CANDLISTS for this group, as specified in the expectation
        # spec. This is a list of lists like
        #
        #   [[x0.adb, y0.adb], [x1.c, y1.c]]
        #
        # where
        #
        # - each sublist is a set of sources to which the set of line
        #   expectations should attach.
        #
        # - exactly one sublist is expected to correspond to sources
        #   we can actually find, which will be _the_ good one.

        self.splist = self.__select_splist_from (candlists=candlists)

        # LXSET: During parsing, a dictionary of LineCX objects corresponding
        # to the stated expectations for the sources we can find, indexed by
        # LRE so we can easily override entries when processing conditional
        # sections.  This is turned into a list eventually, when we know the
        # set of values is fixed and all we need is iterate over values.

        self.lxset = {}

        # UXSET: a list of UnitCX instances, one per unit in the single
        # good list in the CANDLISTS candidates. Computed on close().

        self.uxset = None


    # --------------------------
    # -- Helpers for __init__ --
    # --------------------------

    def __locate_source(self, source):
        """Return valid relative path were SOURCE may be found, searching
        plausible locations uptree from the current point."""

        for pdir in ("../"*n + "src/" for n in range (0, thistest.depth)):
            if os.path.exists(pdir+source):
                return pdir+source

        return None

    def __examine_source_list (self, slist, goodlists):
        """See if all the sources in SLIST can be resolved to existing
        source paths looking uptree. Add the corresponding list of paths
        to GOODLISTS when so."""

        pathlist = []
        for s in slist:
            spath = self.__locate_source (s)
            if not spath:
                return
            else:
                pathlist.append (spath)

        goodlists.append (pathlist)

    def __select_splist_from (self, candlists):
        """Search and return the one good list of units amongst the candidates
        we have."""

        goodlists = []
        [self.__examine_source_list (slist, goodlists)
         for slist in candlists]

        thistest.stop_if (
            len (goodlists) != 1,
            FatalError (
                "goodlists = %d, != 1 for %s" % (
                    len (goodlists), str(candlists)))
            )

        return goodlists[0]

    # -------------------------
    # -- Helpers for close() --
    # -------------------------
    
    def __wrap_lre(self, lx, langinfo):
        """For a source expressed in the language described by LANGINFO,
        adjust line regular expression in LX to expect it prefixed with
        "xx # " where "xx" is the language comment marker."""

        lx.lre = langinfo.comment + " # (" + lx.lre + ")"

        # The parens are crucial here. Consider what would happen for
        # /bla|blo/ without them ...

    # -----------
    # -- close --
    # -----------
    
    def close (self):
        """For each valid unit designated by one of our candidate lists,
        instantiate a UnitCX object and latch the list of instances.
        """

        # Wrap LREs to make sure we look for them in explicit anchors within
        # sources, not as arbitrary sections of source lines. The way to do
        # this depends on the source languages. We assume they are all the
        # same for our list.

        [self.__wrap_lre(
                lx, language_info(self.splist[0]))
         for lx in self.lxset]

        # Now instanciate a unit coverage expectations object for each
        # sourcepath in our list:

        self.uxset = [
            UnitCX(sourcepath=sp, LXset=self.lxset)
            for sp in self.splist
            ]

        return self.uxset

# --------------------
# -- XnotesExpander --
# --------------------

# We need to parse things slightly differently for different languages. For
# example, expectation lines or expectation anchors which start with a comment
# marker.
#
# The comment marker for expectation lines depends on the language where the
# expectation lines are found, which migh be a test driver or a consolidation
# spec).
#
# The comment marker for expectation anchors depends on the language of the
# unit where this anchor will be matched.
#
# For example, a multi language expectations spec in an Ada driver or a
# consolidation spec would look like:
#
#   comment marker for an Ada test driver or a consolidation spec
#   v
#   --# foo.adb
#   --  /foo-ref/  ...  (search for "-- # foo-ref" in foo.adb)
#                                    ^ comment marker for Ada
#   --# bar.c
#   --  /bar-ref/  ...  (search for "// # bar-ref" in bar.c)
#                                    ^ comment marker for C
#
# In a C test driver, the leading comment marker would be "//".

class XnotesExpander:

    # __get_scovdata ()
    # 
    # __parse_scovdata ()
    #   >  __parse_groups_from ()
    #   |   > __try_ctl_update_from ()
    #   |   > __parse_sources ()
    #   |   > __parse_lcx ()
    #   |
    #   >  __resolve_stags_from ()
    #   |   > __resolve_itags_from ()
    #   |   > xnp.instantiate_stag () for all xnote patterns
    #   |
    #   >  g.close () for all parsed groups

    def __init__(self, xfile, xcov_level, covoptions):
        
        self.xfile = xfile
        self.xcov_level = xcov_level

        # Reference controls for CTL lines - compilation options and
        # specific options to gnatcov coverage:

        self.ctls = {
            "%cargs": ' '.join (all_cargs_for_build (thiscargs=None)),
            "%cov"  : ' '.join (covoptions)
            }

        self.xlnotes = {}
        self.xrnotes = {}
        
        [self.to_xnotes(ux) for ux in
         self.__parse_scovdata (self.__get_scovdata (xfile))]

    def to_xnotes(self, ux):
        self.xlnotes [ux.source] = ux.xldict
        self.xrnotes [ux.source] = ux.xrdict

    # --------------------
    # -- __get_scovdata --
    # --------------------

    def __get_scovdata(self, scov_file):
        """Return a list of strings containing the SCOV_data.
        To simplify parsing, the leading comment markers are stripped.
        """

        # The langinfo corresponding to the language of SCOV_FILE
        lang_info = language_info(scov_file)

        # The scov data begins at the first line that starts with the
        # language's comment marker, followed by a '#'. Any line that
        # starts as a comment after this first '#' comment line is assumed
        # to be part of the scov data.  Build a list of lines containing
        # the scov data stored in scov_file now.
        contents = []
        in_scovdata = False
        for line in lines_of(scov_file):
            # Take care of leading/trailing spaces to give the user
            # more flexibility.  Also take care of the trailing new-line
            # character that we get from lines_of.
            line.strip()
            if line.startswith(lang_info.comment + '#'):
                in_scovdata = True
            if in_scovdata and line.startswith(lang_info.comment):
                # Also take this opportunity to strip the leading comment
                # string as well as any space immediately following it.
                # This will simplify the parsing a little bit.
                contents.append(line[len(lang_info.comment):].lstrip())
        return contents

    # -------------------------
    # -- __parse_scovdata () --
    # -------------------------

    def __parse_scovdata(self, scovdata):
        """Parse the given SCOVDATA lines and return the corresponding
        list of UCX instances."""

        uxgroups = self.__parse_groups_from (scovdata)

        # At this point, the groups aren't closed yet and we have something
        # like:
        #
        # uxgroups = [UXgroup (), UXgroup () ...]
        #             v
        #            ---------
        #            .splist = ["p1.adb", "subdir/p2.adb", ...]
        #            .lxset  = [LineCX (), LineCX (), ...]
        #            .           v
        #            .        -------
        #            .        .lre  = "/bla/"
        #            .        .lnp  = XnoteP (...)
        #            .        .rnps = [XnoteP (), XnoteP (), ...]
        #            .                 v
        #            .               --------
        #            .               .kind  = sNoCov
        #            .               .stext = ...
        #            .               .stag  = "<some stag>" 
        #            .
        #            .uxset = None

        # Before closing the groups to trigger the UnitCX creations together
        # with the note instantiations from the XnoteP patterns, we run a name
        # to sloc resolution pass for stags

        self.__resolve_stags_from (uxgroups)

        # Now we can close the groups and construct the complete list
        # of UnitCX instances.

        return [
            ux for uxg in uxgroups for ux in uxg.close()
            ]

    # --------------------------------------------------------
    # -- __parse_groups_from() helper for __parse_scov_data --
    # --------------------------------------------------------

    # First level of group parsing, stopping prior to XnoteP instantiations
    # to allow name -> sloc resolution in between.

    def __parse_groups_from (self, scovdata):

        uxgroups = []

        # We start a new group everytime we see a "sources" line (which starts
        # with '#', after comment markers were stripped).

        current_uxg = None

        grabbing = True

        for line in scovdata:

            (ctl_update, ctl_value) = self.__try_ctl_update_from (line)

            if ctl_update:                

                # A CTL line was found, update our processing state
                # accordingly:

                grabbing = ctl_value

            elif line.startswith('#'):
                                
                # A new group starts. Close the current one first and start
                # grabbing again unconditionally:

                if current_uxg is not None:
                    uxgroups.append (self.__end_parse_on (current_uxg))

                current_uxg = UXgroup (candlists=self.__parse_sources(line))
                grabbing = True

            else:

                # This must be an LX line. Add to the set attached to the
                # current group if we're entitled to. Check lre overriding
                # consistency in any case.

                lx = self.__parse_lcx(line)
                
                thistest.stop_if (
                    lx.lre in current_uxg.lxset and not lx.override,
                    FatalError (
                        "LRE dup without overriding note (%s, %s)" % (
                            lx.lre, self.xfile)
                        )
                    )

                if grabbing:
                    current_uxg.lxset [lx.lre] = lx
                        
        # We're done with all the lines. Close the current group, if any.

        if current_uxg is not None:
            uxgroups.append (self.__end_parse_on (current_uxg))

        return uxgroups

    # -------------------------------------------------------
    # -- __end_parse_on() helper for __parse_groups_from() --
    # -------------------------------------------------------

    # Called whe the first level parsing is done for UXG, prior
    # to instance name resolution and group closing.

    def __end_parse_on (self, uxg):
        uxg.lxset = (
            uxg.lxset.values() + self.__builtin_lcxs_for (uxg)
            )
        return uxg

    # builtin markers support: to let test writers put things like
    #
    #    -- # __l-s-
    #
    # in the functional sources to mean: unless explicitly overriden by
    # a regular expectation spec, expect l- s- for this line always.

    # What default notes we expect for what designator text

    builtin_lxs = {"__l-s-":  "l- ## s-",
                   "__l!d!":  "l! ## d!",
                   "__l!dT-": "l! ## dT-"
                   }

    def __builtin_lcxs_for (self, uxg):
        """Add builtin default LineCX for UXG."""

        # Fetch the explicit line expectations and compute those not there for
        # which we have a default to provide. Beware that the expressions were
        # wrapped by parse_lcx already

        nothere = [lre for lre in self.builtin_lxs if lre not in uxg.lxset]

        # Now compute the list of LCX objects for each of those defaults

        return [
            self.__parse_lcx("/%s/ %s" % (lre, self.builtin_lxs[lre]))
            for lre in nothere
            ]

    # -----------------------------------------------------
    # -- CTL parsing for control of conditional sections --
    # -----------------------------------------------------

    def __try_ctl_update_from (self, line):
        """See if LINE is a CTL line and return the corresponding (do_update,
        now_active) indication tuple. do_update tells if indeed LINE is a CTL
        line and now_active tells whether the whole set of conditional options
        match the current set of active ones."""

        # CTL lines are like "%cov: -S routines ## %cargs: !-gnatn"
        # They are the only lines that may start with '%'.

        if not line.startswith ('%'):
            return (False, None)

        # To keep grabbing LX lines, all the CTL parts must evaluate True.  We
        # always evaluate them all and don't implement shortcircuits. The code
        # remains straightforward this way and we typically have one or two
        # parts only so the potential efficiency gain is not worth any kind of
        # complexification.

        parts = line.split ("##")

        val = True
        for part in parts:
            val &= self.__eval_ctl_update_from (part)

        return (True, val)

    def __eval_ctl_update_from (self, part):
        
        m = re.match (
            pattern=" *(?P<key>%.*?):(?P<opts>.*)\n?", string=part
            )

        key = m.group("key")
        opts = m.group("opts").strip()

        this_val = True
        for oseq in opts.split (','):

            if oseq.startswith ('!'):
                invert = True
                oseq = oseq[1:]
            else:
                invert = False

            optin = re.search (
                pattern=oseq, string=self.ctls[key]
                ) is not None

            this_val &= not optin if invert else optin

        return this_val

    # ------------------------------------------------------
    # -- __parse_sources() helper for __parse_groups_from --
    # ------------------------------------------------------

    def __parse_sources(self, image):
        """Given IMAGE as a string that contains a "sources" line,
        parse that line and return a list of lists, one for each possible
        set of sources expected to apply."""

        # # x0.adb y0.adb | x1.c y1.c
        #
        # -> [["x0.adb", "y0.adb"], ["x1.c", "y1.c"]]
        #
        # to mean "this section applies to (x0.adb and y0.adb) or (x1.c an
        # y1.c), whichever set we can reach from here.

        return [alt.split(' ') for alt in image[1:].strip().split('|')]

    # -----------------------------------------------------------------
    # -- Instance name resolution helpers for __resolve_stags_from() --
    # -----------------------------------------------------------------

    # Instance names resolution to file:line kind of slocs.
    #
    # In s-@(i:NAME), "i:NAME" resolves to "<file>:<line>" when we
    # have ...
    # 
    #          <file>
    #          ...
    # <line>:  <instanciation code here>  -- # i:NAME
    #                                        ^^^^^^^^
    #                                 instanciation marker here

    # The real *source instance* designation marker, that disambiguates
    # separation tags out of -S instances from routine separation tags out of
    # -S routines in expectations, and which we also expect in instantiation
    # line anchors.

    imark = "i:"

    def __resolve_itags_within (self, xnp, idict):

        # xnp.stag contains something like i:NAME1[i:NAME2[i:NAME3]] to
        # designate instantiations

        def __sloc_for (m):
            name = m.group(0)

            # We expect exactly one match for a name so could arrange to stop
            # at the first we find. Having multiple matches can happen out of
            # a test-writer's mistake though, and keeping only one arbitrarily
            # would cause endless confusion so we search them all and issue an
            # error as needed.
            
            slocs = [
                "%s:%d" % (os.path.basename (sp), tl.lno)
                for sp in idict for tl in idict [sp] if name in tl.text
                ]

            thistest.stop_if (
                len (slocs) != 1,
                FatalError ("%d slocs found for stag %s" % (len (slocs), name))
                )

            return slocs[0]

        xnp.stag = re.sub (
            pattern="%s[A-Z_0-9]+" % self.imark,
            repl=__sloc_for, string=xnp.stag
            )

    def __resolve_itags_from (self, all_xnps):
        """Resolve references like "i:NAME" in stags into the file:line
        sloc where an instantiation of NAME is located in the set of sources
        covered by all the units in UXGROUPS."""

        i_xnps = [
            xnp for xnp in all_xnps if xnp.stag and self.imark in xnp.stag
            ]
        
        if len (i_xnps) == 0:
            return

        # Fetch instantiation lines from sources and resolve.  We expect
        # exactly one instantiation per tagged line.

        def __ilines_for (sp):
            tf = Tfile (filename=sp, process=(lambda tl: None))
            return [
                tl for tl in tf.contents() if "# %s" % self.imark in tl.text
                ]

        spaths = [sp for uxg in uxgroups for sp in uxg.splist]
        idict = dict (
            [(sp, __ilines_for (sp)) for sp in spaths])

        [self.__resolve_itags_within (xnp=xnp, idict=idict) for xnp in i_xnps]

    # --------------------------------------------------------
    # -- __resolve_stags_from() helper for __parse_scovdata --
    # --------------------------------------------------------

    def __resolve_stags_from (self, uxgroups):
        """Resolve instance references in separation tags from UXGROUPS, then
        turn separation tag strings into Stag objects."""

        all_xnps = [
            xnp for uxg in uxgroups for lx in uxg.lxset for xnp in lx.rnps
            ]

        self.__resolve_itags_from (all_xnps)

        [xnp.instantiate_stag ()
         for xnp in all_xnps if xnp.stag is not None]

    # --------------------------------------------------------
    # -- Note parsing and selection helpers for __parse_lcx --
    # --------------------------------------------------------

    def __parse_expected_rnotes(self, image):
        if '#' in image:
            imlist = image.split('#')
        elif ',' in image:
            imlist = image.split(',')
        else:
            imlist = [image]
        return [
            rnote for rnote in (
            self.__parse_one_expected_rnote(im.strip())
            for im in imlist) if rnote
            ]

    def __parse_one_expected_rnote(self, image):

        # We have at hand single note spec, possibly conditioned by the
        # xcov-level. Something like "s-", "d=>dT-", or "mu=>c!:"B".

        # We might also have an expected separation tag in any of these
        # cases, e.g. c!:"B"@(my_instance)

        # First fetch the note text that corresponds to our actual xcov-level.
        # If we have no applicable text, state so.

        ntext = self.__select_rnote(image)
        if not ntext:
            return None

        # Otherwise, figure out the note kind + possible column localization +
        # possible instance specialization parts and return a note pattern
        # instance

        if '@(' in ntext:
            (ntext, stag) = ntext.split('@(')
            stag=stag.rstrip(')')
        else:
            stag = None

        if ':' in ntext:
            (ntext, stext) = ntext.split(':"')
            stext=stext.rstrip('"')
        else:
            stext = None

        return XnoteP (
            text=ntext, stext=stext, stag=stag
            )

    def __decode_note_choice(self, text):
        """Given a note_choice that depends potentially on a list of coverage
        levels, return a list of (xcov-level, expected-note-text) tuples that
        represent those dependences.

        For instance, given:

            'u => l!' or 'sd => l+'

        ...this function will return:

            [('stmt+uc_mcdc', 'l!')]
            or
            [('stmt', 'l+'), ('stmt+decision', 'l+')]

        """
        level_from_char = {"s" : "stmt",
                           "d" : "stmt+decision",
                           "m" : "stmt+mcdc",
                           "u" : "stmt+uc_mcdc"}
        result = text.split("=>")

        if len(result) == 1:
            # No choice
            return [("", text)]
        elif len(result) > 2:
            # Parse error
            raise FatalError(
                "Note choice %s contains more than one arrow" % text)
        else:
            note = result[1].lstrip(' ')
            lev_list = result[0].rstrip(' ')

            return [(level_from_char[lchar], note) for lchar in lev_list]

    def __select_lnote(self, text):
        """Decode text to return the line note for the current
        coverage level."""

        lx_lnote_list = [alt.strip() for alt in text.split(',')]

        level_table = dict(
            [ln_tuple for cond_notes in lx_lnote_list
             for ln_tuple in self.__decode_note_choice(cond_notes)])

        if level_table.has_key(self.xcov_level):
            return level_table [self.xcov_level]
        elif level_table.has_key(''):
            return level_table ['']
        else:
            raise FatalError(
                "Missing line expectation choice for level %s in %s"
                % (self.xcov_level, text))


    def __select_rnote(self, text):
        """Decode TEXT into a report note for the current coverage level."""

        # Set of level->note_kind associations in TEXT

        level_table = dict(
            [ln_tuple for ln_tuple in self.__decode_note_choice(text)])

        # If we have one association for the exact level we're running,
        # use that. Fallback to a default kind if we have one.

        if level_table.has_key(self.xcov_level):
            return level_table [self.xcov_level]
        elif level_table.has_key(''):
            return level_table ['']

        return None

    # -----------------
    # -- __parse_lcx --
    # ------------------

    def __parse_lcx(self, image):
        """Parse IMAGE as a string that contains a line expectation
        spec and return the corresponding LineCX object.
        """

        # Extract the various parts of interest from the image.

        m = re.match (
            string  = image,
            pattern = "\s*(?P<lre>=?/.*?/)\s+(?P<lnote>.*) ## (?P<rnotes>.*)"
            )

        if m is None:
            raise FatalError(
                "Invalid '%s' line expectation spec.\n" % image
                + "Expected /LRE/ lnotes ## rnotes")

        lx_lre = m.group("lre")
        
        if lx_lre.startswith('='):
            lre_override = True
            lx_lre = lx_lre [1:]
        else:
            lre_override = False

        lx_lre = lx_lre.strip ('/')

        lx_lnote = XnoteP (
            text=self.__select_lnote (m.group("lnote")), stext=None
            )

        lx_rnotes = self.__parse_expected_rnotes (m.group("rnotes"))

        # If none of the stated report expectation applies to the current
        # xcov-level, default to the empty set:

        if not lx_rnotes:
            lx_rnotes = [XnoteP (text="0", stext=None)]

        # If we have both an empty-set expectation and something else,
        # expectations are wrong. This is a safeguard against a common
        # mistake, thinking, say, that "d=>dT-, 0" means "dT- if
        # --level=stmt+decision, 0 _otherwise_", while it means "dT- etc, 0
        # _always_ (not tied to any particular level)" instead.

        else:

            thistest.stop_if (
                len (lx_rnotes) > 1 and "0" in lx_rnotes,
                FatalError ("Contradictory =report expectation in %s" % image)
            )

        return LineCX (
            lre = lx_lre, lnp = lx_lnote, rnps = lx_rnotes,
            override = lre_override
            )
