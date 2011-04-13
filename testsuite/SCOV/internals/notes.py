# ****************************************************************************
# **                             COVERAGE NOTES                             **
# ****************************************************************************

# What we refer to as or "coverage notes" are abstractions of expected or
# emitted coverage indications.

# We call "Line" or "L" notes the synthetic signs emitted on a line in the
# =xcov outputs, for example, the '+' or '-' signs in the sample below:
#
#   1 .: function In_Range (X , Min, Max : Integer) return Boolean is
#   2 .: begin
#   3 +:    if X < Min then
#   4 -:       return False;
#   5 +:    elsif X > Max then
#   6 -:       return False;
#   7 .:    else
#   8 +:       return True;
#   9 .:    end if;
#  10 .: end;
#
# Lnote objects convey the kind of information emitted ("line fully covered"
# for '+', "line not covered" for '-') and the line number for which it was
# emitted. They are collected in outer data structures (e.g. dictionaries) to
# bind them with the source file where they were found.

# We call "Report", or "R" notes the indications emitted for slocs in the
# =report outputs, for example:
#
#  in_range.adb:4:7: statement not executed
#  in_range.adb:6:7: statement not executed
#
# As for Lnotes, Rnote objects convey the kind of indication emitted, the sloc
# for which they were emitted, and are gathered in outer data structures to
# bind them with the source file from which they originate. Different possible
# degrees of precision could be present in the source location designation:
#
#   * for a specific point in the source, e.g. "4:5" for "column 5 on line #4"
#   * for a line as a whole, e.g. "4" or "4:0" for "line #4",
#   * for a line segment, e.g. "4:7-9" for "columns 7 to 9 on line #4"
#   * for a section of source spanning multiple lines,
#     e.g. "3:4-9:7" for "column #4 on line #3 to column #7 on line #9",
#
# To let us evaluate if some reported coverage indication corresponds to an
# expectation, we abstract all these with variations over sloc SECTIONS which
# we can easily check for inclusion within each other. We manipulate general
# sloc SECTIONS, possibly specialized as line SEGMENTs (section with start and
# end on the same line) or full LINEs (segment from first to last column) or
# POINTs (one column segments).

# Expected indications are expressed as comments in single test
# drivers like:
#
# -- test_blob1.adb --
# procedure test_blob1 is
# begin
#   ...
# end;
#
# --# blob.adb       <= start of expectations for unit blob.adb
#
#       o---------- "line regular expression"
#       |    o------ expected Line note for matching lines
#       |    |  o--- set of expected Report notes for matching lines
#       |    |  |
#       v    v  v
# --  /foo/  l- s-   <= for lines matching "-- # foo", expect
#                       a '-' synthetic note on the =xcov line (l-)
#                       a 'statement not covered' =report indication (s-)
#
# --  /bar/  l+ 0    <= for lines matching "-- # bar", expect
#                       a '+' synthetic note on the =xcov line (l+)
#                       no =report indication (0 = empty set)
# --# otherunit.adb
# --  ...
#
# We typically use 'X' in names to denote eXpectation, so will typically
# have Xnote objects to represent the set of expected indications after
# the line patterns were matched against a source.

from SUITE.context import thistest
from SUITE.utils import lines_of, FatalError

from . segments import *
from . tfiles import Tfile

# Symbolic values & names for kinds of coverage notes:
# ----------------------------------------------------

# lNoCode  : no code for line (=xcov)
# lFullCov : full coverage for line (=xcov)
# lx0      : line part of exempted block, 0 deviations (=xcov)
# lx1      : line part of exempted block, >0 deviations (=xcov)
# lNoCov   : line not covered (=xcov)
# lPartCov : line partially covered (=xcov)

# sNoCov   : stmt not covered (=report)
# sPartCov : unable to assess precise stmt coverage (=report)
# dtNoCov  : decision outcome True not covered (=report)
# dfNoCov  : decision outcome False not covered (=report)
# dPartCov : one decision outcome not covered (=report)
# dNoCov   : decision never evaluated (=report)

# cPartCov : independent effect of condition not demonstrated (=report)

# xBlock0  : exempted block, 0 deviations (=report)
# xBlock1  : exempted block, >0 deviations (=report)

lNoCode, lFullCov, \
strictNote, \
lx0, lx1, \
deviationNote, \
lNoCov, lPartCov, \
sNoCov, sPartCov, \
dtNoCov, dfNoCov, dPartCov, dNoCov, cPartCov, \
blockNote, \
xBlock0, xBlock1 = range(18)

# DEVIATION notes are those representing violations of a coverage mandate
# associated with a general criterion.

def deviation_p(nkind):
    return nkind > deviationNote and nkind < blockNote

# POSITIVE notes are those representing a positive statement about a
# coverage mandate, only present in =xcov outputs.

def positive_p(nkind):
    return nkind == lFullCov

# BLOCK notes are those emitted as a single note for a block of code in
# =report outputs,

def block_p(nkind):
    return nkind > blockNote

# STRICT notes are those for which an exact match between reports and
# expectations is required: an expected note should be reported (errout
# otherwise, unless the expectation is explicitely tagged weak), and a
# reported note should be expected (errout otherwise).

# !STRICT notes should also be reported when expected (or err unless weak
# expectation), but trigger no err when reported eventhough not expected.

def strict_p(nkind):
    return nkind > strictNote


NK_image  = {None: "None",
             lNoCode: "lNoCode", lFullCov: "lFullCov",
             lNoCov: "lNoCov", lPartCov: "lPartCov",
             lx0: "lx0", lx1: "lx1",
             sNoCov: "sNoCov", sPartCov: "sPartCov",
             dtNoCov: "dtNoCov", dfNoCov: "dfNoCov",
             dPartCov: "dPartCov", dNoCov: "dNoCov",
             xBlock0: "xBlock0", xBlock1: "xBlock1",
             cPartCov: "cPartCov"}

# Useful sets of note kinds:
# --------------------------

lNoteKinds = (lNoCode, lNoCov, lPartCov, lFullCov, lx0, lx1)

sNoteKinds = (sNoCov, sPartCov)
dNoteKinds = (dtNoCov, dfNoCov, dPartCov, dNoCov)
cNoteKinds = (cPartCov,)

xNoteKinds = (xBlock0, xBlock1)

# Note kinds that can be associated to one of xcov's message, independantly
# of the context of invocation.
rNoteKinds = sNoteKinds+dNoteKinds+cNoteKinds+xNoteKinds

# Relevant/Possible Line and Report notes for CATEGORY/CONTEXT:
# -------------------------------------------------------------

rp_lnotes_for = { "stmt":     lNoteKinds,
                  "decision": lNoteKinds,
                  "mcdc":     lNoteKinds
                }

rp_rnotes_for = { "stmt":     xNoteKinds+sNoteKinds,
                  "decision": xNoteKinds+sNoteKinds+dNoteKinds,
                  "mcdc":     xNoteKinds+sNoteKinds+dNoteKinds+cNoteKinds
                }

# Note that we do care about exemptions at every level and need to watch out
# for subtle changes in the number of violations exempted when running a given
# test in different contexts (for different target levels).

# ====================
# == Coverage Notes ==
# ====================

# Report section identifiers, to let us control when looking for indication
# patterns and check that each appears in the section where we expect it.

rsNoInterest, rsNotExempted, rsExempted = range (3)

RS_image = {None: "None",
            rsNoInterest: "rsNoInterest",
            rsNotExempted: "rsNotExempted",
            rsExempted: "rsExempted"}

# -----------
# -- Cnote --
# -----------

# Some precise coverage note, either expected or reported:

class Cnote:
    def __init__(self, kind):

        # Kind of note, line segment and report section id.

        self.kind = kind
        self.segment = None
        self.rsid = None

        # An expected note for one segment will be discharged by an emitted
        # note of the same kind for a tighter segment, and the emitted note
        # will have to be found in the expected report section. =xcov reports
        # are considered section-less, and rsid remains None in this case.

# -----------
# -- Xnote --
# -----------

# Expected note, as instanciated by an expectation pattern over a real
# source line:

class Xnote (Cnote):
    def __init__(self, xnp, block):
        Cnote.__init__ (self, xnp.kind)
        self.weak = xnp.weak
        self.block = block

        self.stext = xnp.stext
        self.nmatches = 0

        self.discharger = None  # The Enote that discharged this

        # Determine our expected segment id. Simple enough not to warrant
        # a class specialization by itself.

        if self.kind in lNoteKinds:
            self.rsid = None
        elif self.kind in xNoteKinds:
            self.rsid = rsExempted
        else:
            self.rsid = rsNotExempted

    def register_match(self, segment):
        self.segment = segment
        self.nmatches += 1

# ------------
# -- XnoteP --
# ------------

# Expectation pattern, materializing the user level representation of a note
# expectation in drivers (text like "l+"). These get instanciated into a set
# of actual expected indications for precise segments when the line regular
# expressions are matched.

# Different kind of expectations instanciate differently on a given source
# line. We introduce specialized note factories for this purpose:

# Block notes are relevant for a general section. Eventhough the block is
# matched line by line, we need to materialize a single note for the whole
# block.

class XnoteP_block:

    def __init__(self, notep):
        self.notep  = notep
        self.lastni = None    # The last note instance we returned

    def instanciate_over(self, tline, block):

        # We create a single instance the first time around, then expand the
        # section over subsequence matches.

        if self.lastni:
            thisni = None
            self.lastni.segment.sloc1.l = tline.lno

        else:
            thisni = Xnote (xnp=self.notep, block=block)
            thisni.register_match (Section(
                    l0 = tline.lno, c0 = 0, l1 = tline.lno, c1 = 0))

        if thisni: self.lastni = thisni
        return thisni

# !block notes without a specific segment text are relevant to entire lines

class XnoteP_line:

    def __init__(self, notep):
        self.notep = notep

    def instanciate_over(self, tline, block):

        thisni = Xnote (xnp=self.notep, block=block)
        thisni.register_match (Line(tline.lno))

        return thisni

# !block notes with a specific segment subtext are relevant to that segment:
# we'll expect a reported note to designate a point within that subtext (most
# often, the beginning of it)

class XnoteP_segment:

    def __init__(self, notep, stext):
        self.notep = notep
        self.stext = stext

    def instanciate_over(self, tline, block):

        thisni = Xnote (xnp=self.notep, block=block)

        # Register matches for Segments corresponding to all the instances
        # of the subtext we find, then error out if too few or too many.

        [thisni.register_match (Segment (tline.lno, m.start()+1, m.end()))
         for m in re.finditer (self.stext, tline.text)]

        thistest.stop_if (
            thisni.nmatches == 0, FatalError (
                "couldn't find subtext '%s' in line '%s'"
                % (self.stext, tline.text)))

        thistest.stop_if (
            thisni.nmatches > 1, FatalError (
                "multiple matches of subtext '%s' in line '%s'"
                % (self.stext, tline.text)))

        return thisni

class XnoteP:

    NK_for = {'l.': lNoCode, 'l-': lNoCov, 'l!': lPartCov, 'l+': lFullCov,
              'l#': lx0, 'l*': lx1,
              's-': sNoCov, 's!': sPartCov,
              'dT-': dtNoCov, 'dF-': dfNoCov, 'd!': dPartCov, 'd-':dNoCov,
              'c!': cPartCov,
              'x0': xBlock0, 'x+': xBlock1,
              '0': None}

    def __init__(self, text, stext=None):
        self.weak = text[0] == '~'
        if self.weak: text = text[1:]

        self.kind = self.NK_for[text]
        self.stext = stext

        # We could require and use stext to store expected justification text
        # for exemptions. We don't handle that as of today.

        thistest.stop_if (
            False and self.stext == None and self.kind in xNoteKinds,
            FatalError ("expected justification text required for %s" % text))

        # Setup our instanciation factory now, which lets us perform the
        # required test only once:

        if block_p (self.kind):
            self.factory = XnoteP_block (notep=self)
        elif not self.stext:
            self.factory = XnoteP_line (notep=self)
        else:
            self.factory = XnoteP_segment (notep=self, stext=stext)

    def instanciate_over (self, tline, block):
        return self.factory.instanciate_over (tline, block)

# -----------
# -- Enote --
# -----------

# Emitted note, as extracted from an xcov report:

class Enote(Cnote):
    def __init__(self, kind, segment, rsid=None):
        self.kind = kind        # The kind of emitted note
        self.segment = segment  # The line segment it designates

        self.discharges = None  # The Xnote it discharges
        self.rsid = rsid        # The report section where this was found


# ---------------
# -- KnoteDict --
# ---------------

# Dictionary of coverage notes indexed by note kind:

class KnoteDict(dict):
    def __init__(self, possible_keys):
        [self.__setitem__(key, []) for key in possible_keys]

    def register(self, note):
        self[note.kind].append (note)


