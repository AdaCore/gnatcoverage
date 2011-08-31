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

# Expected indications are expressed as comments in single test drivers, like:
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

# For =report outputs, violation expectations, such as "s-", map one-to-one
# with some indication we ought to find in a coverage assessment report.
#
# Conversely, the "0" empty set denotes the expectation of an absence of
# reported violations, corresonding to a full set of possible indications.
#
# We qualify "0" as an anti-expectation.

# ****************************************************************************

# Symbolic values & names for kinds of coverage notes:
# ----------------------------------------------------

# lNoCode  : no code for line (=xcov)
# lFullCov : full coverage for line (=xcov)
# r0       : expect empty set of violations (=report)
# r0c      : like r0, on a statement continuation line (=report)
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

# etNoCov  : expression outcome True not covered (=report)
# efNoCov  : expression outcome False not covered (=report)
# ePartCov : one expression outcome not covered (=report)
# eNoCov   : expression never evaluated (=report)

# cPartCov : independent effect of condition not demonstrated (=report)

# xBlock0  : exempted block, 0 deviations (=report)
# xBlock1  : exempted block, >0 deviations (=report)

# Tansient kinds: these may be used in expectations and should always be
# subject to substitution rules mapping them to other kinds. No emitted note
# will ever match them. These are useful for shared drivers when the actual
# kind of expectation depends on the functional code.

# Expression vs Control decisions:

# otNoCov  : outcome True not covered (=report)
# ofNoCov  : outcome False not covered (=report)
# oPartCov : one outcome not covered (=report)
# oNoCov   : outcome never evaluated (=report)

lNoCode, lFullCov, \
strictNote, \
r0, r0c, lx0, lx1, \
deviationNote, \
lNoCov, lPartCov, \
sNoCov, sPartCov, \
dtNoCov, dfNoCov, dNoCov, dPartCov, \
etNoCov, efNoCov, eNoCov, ePartCov, \
otNoCov, ofNoCov, oNoCov, oPartCov, \
cPartCov, \
blockNote, \
xBlock0, xBlock1 = range(28)

NK_image  = {None: "None",
             lNoCode: "lNoCode", lFullCov: "lFullCov",
             lNoCov: "lNoCov", lPartCov: "lPartCov",
             r0 : "r0", r0c: "r0c", lx0: "lx0", lx1: "lx1",
             sNoCov: "sNoCov", sPartCov: "sPartCov",
             dtNoCov: "dtNoCov", dfNoCov: "dfNoCov", dNoCov: "dNoCov",
             dPartCov: "dPartCov",
             etNoCov: "etNoCov", efNoCov: "efNoCov", eNoCov: "eNoCov",
             ePartCov: "ePartCov",
             otNoCov: "otNoCov", ofNoCov: "ofNoCov", oNoCov: "oNoCov",
             oPartCov: "oPartCov",
             xBlock0: "xBlock0", xBlock1: "xBlock1",
             cPartCov: "cPartCov"}

# ===============================
# == Useful sets of note kinds ==
# ===============================

# Line notes (=xcov); the set of possible expectations matches the
# set of possible emitted indications

elNoteKinds = (lNoCode, lNoCov, lPartCov, lFullCov, lx0, lx1)
xlNoteKinds = elNoteKinds

# Report notes (=report), which feature anti-expectations that
# explicitely state expection of absence of emitted notes

sNoteKinds = ( # SC violations
    sNoCov, sPartCov)

dNoteKinds = ( # DC violations
    dtNoCov, dfNoCov, dPartCov, dNoCov)

cNoteKinds = ( # MCDC violations
    etNoCov, efNoCov, ePartCov, eNoCov,  cPartCov)

xNoteKinds = (xBlock0, xBlock1)                     # Exemption regions

rAntiKinds = (r0, r0c)                              # Anti-expectations

tNoteKinds = (otNoCov, ofNoCov, oPartCov, oNoCov)   # Transient kinds

# Even though they are expected never to be emitted, we include the transient
# kinds in the Emitted Report Notes set because we do want to handle them as
# if they could be emitted and report them as unmatched.

erNoteKinds = sNoteKinds+dNoteKinds+cNoteKinds+xNoteKinds+tNoteKinds
xrNoteKinds = erNoteKinds+rAntiKinds

# ==========================
# == Note Kind Predicates ==
# ==========================

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

# ANTI expectations are those that explicitly state that we expect absence
# of emitted indications

def anti_p(nkind):
    return nkind in rAntiKinds

# ===========================
# == Coverage Note Classes ==
# ===========================

# -----------
# -- Block --
# -----------

# Almost empty class that helps materialize source regions to which expected
# coverage note instances belong. Instanciated while reading sources when
# expected note patterns are processed. Our purpose is only to associate notes
# with regions, for which an object id is enough + a parent link to represent
# nesting trees.

class Block:
    def __init__(self, parent):
        self.parent = parent

# -----------
# -- Cnote --
# -----------

# Some precise coverage note, either expected or reported:

class Cnote:
    def __init__(self, kind):

        # Kind of note, line segment and report section id.

        self.kind = kind
        self.segment = None

        # An expected note for one segment will be discharged by an emitted
        # note of the same kind for a tighter segment. The emitted note will
        # have to be found in the expected report section. =xcov reports are
        # considered section-less.

# -----------
# -- Xnote --
# -----------

# Expected note, as instanciated by an expectation pattern over a real
# source line:

class Xnote (Cnote):

    def __init__(self, xnp, block, kind):
        Cnote.__init__ (self, kind)
        self.weak = xnp.weak
        self.block = block

        self.stext = xnp.stext
        self.nmatches = 0

        self.discharger = None  # The Enote that discharged this

    def register_match(self, segment):
        """Register that this instance matched SEGMENT for a source line.
        Increase the number of such matches and remember only the last."""

        self.nmatches += 1
        self.segment = segment

    def satisfied(self):
        """Tell whether this [anti-]expectation is satisfied at this point."""

        if anti_p(self.kind):
            return self.discharger == None
        else:
            return self.discharger != None

# -----------
# -- Enote --
# -----------

# Emitted note, as extracted from an xcov report:

class Enote(Cnote):
    def __init__(self, kind, segment, source):
        self.kind = kind        # The kind of emitted note
        self.segment = segment  # The line segment it designates
        self.source = source    # The corresponding source name
        self.discharges = None  # The Xnote it discharges

# ---------------
# -- KnoteDict --
# ---------------

# Dictionary of coverage notes indexed by note kind:

class KnoteDict(dict):
    def __init__(self, possible_keys):
        [self.__setitem__(key, []) for key in possible_keys]

    def register(self, note):
        self[note.kind].append (note)


