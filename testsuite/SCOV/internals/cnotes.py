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
#       o------------- "line regular expression"
#       |    o--------- expected Line note for matching lines
#       |    |     o--- set of expected Report notes for matching lines
#       |    |     |
#       v    v     v
# --  /foo/  l- ## s-   <= for lines matching "-- # foo", expect
#                          a '-' synthetic note on the =xcov line (l-)
#                          a 'statement not covered' =report indication (s-)
#
# --  /bar/  l+ ## 0    <= for lines matching "-- # bar", expect
#                          a '+' synthetic note on the =xcov line (l+)
#                          no =report indication (0 = empty set)
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

# lNoCode       : no code for line (=xcov)
# lNotCoverable : no code but statement on line, hence not coverable (=xcov)
# lUndetCov     : scos present but could not determine coverage state (=xcov)
# lDisCov       : coverage analysis disabled for the line (=xcov)
# lFullCov      : full coverage for line (=xcov)
# r0            : expect empty set of violations (=report)
# r0c           : like r0, on a statement continuation line (=report)
# lx0           : line part of exempted block, 0 deviations (=xcov)
# lx1           : line part of exempted block, >0 deviations (=xcov)
# lx2           : line part of exempted block, >0 undet. cov. items (=xcov)
# lx            : line part of exempted block, do not check deviations (=xcov)
# lNoCov        : line not covered (=xcov)
# lPartCov      : line partially covered (=xcov)

# sNotCoverable : no code but statement on line, hence not coverable (=report)
# sUndetCov     : statement with undetermined coverage state (=report)
# sNoCov        : stmt not covered (=report)
# sPartCov      : unable to assess precise stmt coverage (=report)

# dtAlways  : decision is always True, hence not coverable (=report)
# dfAlways  : decision is always False, hence not coverable (=report)
# dtNoCov   : decision outcome True not covered (=report)
# dfNoCov   : decision outcome False not covered (=report)
# dPartCov  : one decision outcome not covered (=report)
# dNoCov    : decision never evaluated (=report)
# dUndetCov : decision with undetermined coverage state for decision
#             coverage (=report)

# etNoCov   : expression outcome True not covered (=report)
# efNoCov   : expression outcome False not covered (=report)
# ePartCov  : one expression outcome not covered (=report)
# eNoCov    : expression never evaluated (=report)
# eUndetCov : expression with undetermined coverage state for MC/DC
#             coverage (=report)

# cPartCov : independent effect of condition not demonstrated (=report)

# aNoCov    : assertion never evaluated (=report)
# atNoCov   : assertion expression outcome TRUE never evaluated (=report)
# acPartCov : assertion condition was never evaluated during an evaluation of
#             the decision to True (=report)

# fNoCov    : subprogram never entered (=report)
# cNoCov    : call statement or expression never evaluated (=report)
# fUndetCov : subprogram with undetermined coverage state (=report)
# cUndetCov : call with undetermined coverage state (=report)

# xBlock0  : exempted block, 0 deviations (=report)
# xBlock1  : exempted block, >0 deviations (=report)
# xBlock2  : exempted block, >0 undetermined coverage items (=report)

# dBlock   : disabled block

# Transient kinds: these may be used in expectations and should always be
# subject to substitution rules mapping them to other kinds. No emitted note
# will ever match them. These are useful for shared drivers when the actual
# kind of expectation depends on the functional code.

# Expression vs Control decisions:

# otNoCov  : outcome True not covered (=report)
# ofNoCov  : outcome False not covered (=report)
# oPartCov : one outcome not covered (=report)
# oNoCov   : outcome never evaluated (=report)

# Exempted violations. These are a subset of the violations described above
# that are possible to find in exempted blocks. They have the same meaning but
# are simply prefixed by an uppercase 'X' to express that they can only be
# found in exempted regions. The relevant notes are:
#
# XsNoCov, XsPartCov, XsNotCoverable, XsUndetCov, XotNoCov, XofNoCov,
# XoPartCov, XoNoCov, XcPartCov, r0, r0c

# Annotations lower than strictNote won't trigger an unexpected annotation
# failure if they appear in a place where they are not explicitly expected.

(
    lNoCode,
    lFullCov,
    lx,
    strictNote,
    r0,
    r0c,
    lx0,
    lx1,
    lx2,
    deviationNote,
    lNoCov,
    lPartCov,
    lNotCoverable,
    lUndetCov,
    lDisCov,
    sNoCov,
    sPartCov,
    sNotCoverable,
    sUndetCov,
    dtAlways,
    dfAlways,
    dNotCoverable,
    dtNoCov,
    dfNoCov,
    dNoCov,
    dPartCov,
    dUndetCov,
    etNoCov,
    efNoCov,
    eNoCov,
    ePartCov,
    eUndetCov,
    otNoCov,
    ofNoCov,
    oNoCov,
    oPartCov,
    cPartCov,
    XsNoCov,
    XsPartCov,
    XsNotCoverable,
    XsUndetCov,
    XotNoCov,
    XofNoCov,
    XoPartCov,
    XoNoCov,
    XcPartCov,
    Xr0,
    Xr0c,
    aNoCov,
    atNoCov,
    acPartCov,
    fNoCov,
    cNoCov,
    fUndetCov,
    cUndetCov,
    blockNote,
    xBlock0,
    xBlock1,
    xBlock2,
    dBlock,
) = range(60)

NK_image = {
    None: "None",
    lNoCode: "lNoCode",
    lNotCoverable: "lNotCoverable",
    lUndetCov: "lUndetCov",
    lDisCov: "lDisCov",
    lFullCov: "lFullCov",
    lNoCov: "lNoCov",
    lPartCov: "lPartCov",
    r0: "r0",
    r0c: "r0c",
    lx0: "lx0",
    lx1: "lx1",
    lx2: "lx2",
    lx: "lx",
    sNoCov: "sNoCov",
    sPartCov: "sPartCov",
    sNotCoverable: "sNotCoverable",
    sUndetCov: "sUndetCov",
    dtAlways: "dtAlways",
    dfAlways: "dfAlways",
    dNotCoverable: "dNotCoverable",
    dtNoCov: "dtNoCov",
    dfNoCov: "dfNoCov",
    dNoCov: "dNoCov",
    dPartCov: "dPartCov",
    dUndetCov: "dUndetCov",
    etNoCov: "etNoCov",
    efNoCov: "efNoCov",
    eNoCov: "eNoCov",
    ePartCov: "ePartCov",
    eUndetCov: " eUndetCov",
    otNoCov: "otNoCov",
    ofNoCov: "ofNoCov",
    oNoCov: "oNoCov",
    oPartCov: "oPartCov",
    xBlock0: "xBlock0",
    xBlock1: "xBlock1",
    xBlock2: "xBlock2",
    cPartCov: "cPartCov",
    XsNoCov: "XsNoCov",
    XsPartCov: "XsPartCov",
    XsNotCoverable: "XsNotCoverable",
    XsUndetCov: "XsUndetCov",
    XotNoCov: "XotNoCov",
    XofNoCov: "XofNoCov",
    XoPartCov: "XoPartCov",
    XoNoCov: "XoNoCov",
    XcPartCov: "XcPartCov",
    Xr0: "Xr0",
    Xr0c: "Xr0c",
    aNoCov: "aNoCov",
    atNoCov: "atNoCov",
    acPartCov: "acPartCov",
    fNoCov: "fNoCov",
    cNoCov: "cNoCov",
    fUndetCov: "fUndetCov",
    cUndetCov: "cUndetCov",
    dBlock: "dBlock",
}


# ===============================
# == Useful sets of note kinds ==
# ===============================

# Line notes (=xcov); the set of possible expectations matches the
# set of possible emitted indications

elNoteKinds = (
    lNoCode,
    lNotCoverable,
    lUndetCov,
    lDisCov,
    lNoCov,
    lPartCov,
    lFullCov,
    lx0,
    lx1,
    lx2,
)

xlTransparentKinds = (lx,)

xlNoteKinds = elNoteKinds + xlTransparentKinds

# Report notes (=report), which feature anti-expectations that
# explicitely state expection of absence of emitted notes

# SC indications
sNoteKinds = (sNoCov, sPartCov, sNotCoverable, sUndetCov)

# DC indications
dNoteKinds = (
    dtNoCov,
    dfNoCov,
    dPartCov,
    dNoCov,
    dtAlways,
    dfAlways,
    dNotCoverable,
    dUndetCov,
)

# MCDC violations
cNoteKinds = (etNoCov, efNoCov, ePartCov, eNoCov, cPartCov, eUndetCov)

# Assertion violations
atcNoteKinds = (aNoCov, atNoCov)
atccNoteKinds = acPartCov
aNoteKinds = (aNoCov, atNoCov, atccNoteKinds)

# Function and call coverage violations
fNoteKinds = (fNoCov, cNoCov, fUndetCov, cUndetCov)

# Exemption regions
xNoteKinds = (xBlock0, xBlock1, xBlock2)


# Exempted violations
XsNoteKinds = (XsNoCov, XsPartCov, XsNotCoverable, XsUndetCov)

XoNoteKinds = (XotNoCov, XofNoCov, XoPartCov, XoNoCov)

XcNoteKinds = (XcPartCov,)

XrAntiKinds = (Xr0, Xr0c)

XNoteKinds = XsNoteKinds + XoNoteKinds + XcNoteKinds

# Disabled coverage regions
disabledNoteKinds = (dBlock,)

# Anti-expectations
rAntiKinds = (r0, r0c)

# Transient kinds
tNoteKinds = (otNoCov, ofNoCov, oPartCov, oNoCov)

# Even though they are expected never to be emitted, we include the transient
# kinds in the Emitted Report Notes set because we do want to handle them as
# if they could be emitted and report them as unmatched.

erNoteKinds = (
    sNoteKinds
    + dNoteKinds
    + cNoteKinds
    + xNoteKinds
    + tNoteKinds
    + XNoteKinds
    + disabledNoteKinds
)
erNoteKinds += aNoteKinds + fNoteKinds
xrNoteKinds = erNoteKinds + rAntiKinds + XrAntiKinds


# ==========================
# == Note Kind Predicates ==
# ==========================


def deviation_p(nkind):
    """
    DEVIATION notes are those representing violations of a coverage mandate
    associated with a general criterion.
    """
    return nkind > deviationNote and nkind < blockNote


def positive_p(nkind):
    """
    POSITIVE notes are those representing a positive statement about a coverage
    mandate, only present in =xcov outputs.
    """
    return nkind == lFullCov


def block_p(nkind):
    """
    BLOCK notes are those emitted as a single note for a block of code in
    =report outputs,
    """
    return nkind > blockNote


def strict_p(nkind):
    """
    STRICT notes are those for which an exact match between reports and
    expectations is required: an expected note should be reported (errout
    otherwise, unless the expectation is explicitely tagged weak), and a
    reported note should be expected (errout otherwise).

    !STRICT notes should also be reported when expected (or err unless weak
    expectation), but trigger no err when reported eventhough not expected.
    """
    return nkind > strictNote


def anti_p(nkind):
    """
    ANTI expectations are those that explicitly state that we expect absence of
    emitted indications.
    """
    return nkind in rAntiKinds


def transparent_p(nkind):
    """
    TRANSPARENT expectations are those that should not produce an expected note
    to be matched. It is relevant for exempted regions. For an exempted region,
    we have one _line_ note emitted for each line of the block, one _report_
    block note emitted for the entire region and one _report_ note for each
    exempted violation within the region. For example:

    foo.adb.xcov:
        2 *:   pragma Annotate (Exempt_On, "comment"); -- # ex-region
        3 *:   if Debug then                           -- # ex-region-test
        4 *:      Count := Count + 1;                  -- # ex-region-bump
        5 *:   end if;                                 -- # ex-region
        6 *:   pragma Annotate (Exempt_Off);           -- # ex-region

    report:
        -- Exempted Regions --
        foo.adb:2:4-6:4: 2 exempted violations, justification:
        "comment"
        foo.adb:3:7 decision outcome True never exercised
        foo.adb:4:7 statement not executed

    In expectation blocks, the line intended to match the emitted report block
    note for the entire region is typically designed to match on all the lines
    of the region to capture the sloc range. For example:

    --# foo.adb
    --# /ex-region/ l* ## x1

    This matches all the lines, from 2 to 6, and creates a "l*" line note
    expectation for each line that is indeed discharged by the .xcov output.

    Now we need additional expectations for the emitted violation _report_
    notes, and we need to prevent these from creating new line notes
    expectations that would never be discharged. On our example, this would be
    achieved with:

    --# /ex-region-test l= ## dT-
    --# /ex-region-bump l= ## s-

    where the "l=" expectation is "transparent".
    """

    return nkind in xlTransparentKinds


# ===========================
# == Coverage Note Classes ==
# ===========================


class Block:
    """
    Source regions to which expected coverage note instances belong.

    Instanciated while reading sources when expected note patterns are
    processed. Our purpose is only to associate notes with regions, for which
    an object id is enough + a parent link to represent nesting trees.
    """

    def __init__(self, parent):
        self.parent = parent


class Cnote:
    """Some precise coverage note, either expected or reported."""

    def __init__(self, kind):
        # Kind of note, line segment and report section id
        self.kind = kind
        self.segment = None

        # An expected note for one segment will be discharged by an emitted
        # note of the same kind for a tighter segment. The emitted note will
        # have to be found in the expected report section. =xcov reports are
        # considered section-less.

        # In addition, any note might have an associated "separation tag" to
        # provide finer grained diagnostics in presence of inlining or generic
        # instances.
        self.stag = None

    def image(self):
        return "%s%s mark at %s" % (
            NK_image[self.kind],
            "(from %s)" % self.stag.text if self.stag else "",
            self.segment,
        )


class Xnote(Cnote):
    """
    Expected note, as instanciated by an expectation pattern over a real source
    line.
    """

    def __init__(self, xnp, block, kind):
        Cnote.__init__(self, kind)
        self.weak = xnp.weak
        self.block = block

        self.stext = xnp.stext
        self.stag = xnp.stag
        self.nmatches = 0

        self.discharger = None  # The Enote that discharged this

    def register_match(self, segment):
        """
        Register that this instance matched SEGMENT for a source line.
        Increase the number of such matches and remember only the last.
        """
        self.nmatches += 1
        self.segment = segment

    def satisfied(self):
        """Tell whether this [anti-]expectation is satisfied at this point."""
        if anti_p(self.kind):
            return self.discharger is None
        else:
            return self.discharger is not None


class Enote(Cnote):
    """Emitted note, as extracted from an xcov report."""

    def __init__(self, kind, segment, source, stag=None):
        Cnote.__init__(self, kind)
        self.segment = segment  # The line segment it designates
        self.source = source  # The corresponding source name
        self.stag = stag  # The separation tag it contains

        self.discharges = None  # The Xnote it discharges


class KnoteDict(dict):
    """Dictionary of coverage notes indexed by note kind."""

    def __init__(self, possible_keys):
        self.update((key, []) for key in possible_keys)

    def register(self, note):
        self[note.kind].append(note)
