"""
EMITTED REPORT NOTE EXPANDERS

Expose the RnotesExpander class which constructs a { source -> KnoteDict }
dictionary of emitted Line Notes from a provided =report outputs.
"""

import re

from SUITE.context import thistest
from .cnotes import (
    xBlock0,
    xBlock1,
    sNoCov,
    sPartCov,
    sNotCoverable,
    dfAlways,
    dtAlways,
    dNotCoverable,
    dfNoCov,
    dtNoCov,
    dNoCov,
    dPartCov,
    efNoCov,
    etNoCov,
    eNoCov,
    ePartCov,
    cPartCov,
    aNoCov,
    atNoCov,
    acPartCov,
    fNoCov,
    cNoCov,
    fUndetCov,
    cUndetCov,
    Enote,
    KnoteDict,
    erNoteKinds,
    sUndetCov,
    dUndetCov,
    eUndetCov,
    xBlock2,
    XsNoCov,
    XsPartCov,
    XsNotCoverable,
    XsUndetCov,
    XotNoCov,
    XofNoCov,
    XoPartCov,
    XoNoCov,
    XcPartCov,
    dBlock,
)
from .segments import Sloc, Sloc_from_match
from .stags import Stag_from
from .tfiles import Tfile


# =========================================
# == Report internal abstraction classes ==
# =========================================

# The central abstraction we use is that of a report "Contents Block", which
# models a part of the report containing real contents displayed as a sequence
# of text lines without intermediate block headers.
#
# This encompasses sections within chapters, such as the STMT COVERAGE
# violations section in:
#
# ============================            <- Report *chapter* header here.
# == 2. COVERAGE VIOLATIONS ==            <- No immediate contents, so no
# ============================            <- "contents block" representation
#
# 2.1. STMT COVERAGE                      <- Report *section* header here.
# ------------------                      <-
#
# expr.adb:8:10: statement not executed   <- Immediate contents, so section
#                                         <- mapped to an internal "contents
# 1 violation.                            <- block" representation.
#
#
# Or chapters without intermediate sections, such as the ANALYSIS SUMMARY
# chapter in:
#
# =========================               <- Report *chapter* header here.
# == 3. ANALYSIS SUMMARY ==               <-
# =========================               <-
#
# 1 STMT violation.                       <- Immediate contents, so chapter
# 2 DECISION violations.                  <- mapped to an internal "contents
#                                         <- block" representation.


class Rdiagline:
    """
    Abstract diagnostic line, a-la "p.adb:8:4 statement not executed". This is
    a general Sloc followed by some diagnostic text.
    """

    def __init__(self, sloc, diag):
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


def Rdiagline_from(text):
    p = re.match(Rdiagline.re, text)

    return (
        Rdiagline(sloc=Sloc_from_match(p), diag=p.group("diag")) if p else None
    )


class Rblock:
    """
    The base report contents block abstraction, from which a lot of
    specializations will derive.
    """

    def __init__(self, name, re_start, re_end):
        # NAME holds the text with which this block will be referred to in
        # error messages. RE_START/RE_END are regexps which tells whether a
        # line of text matches the expected header line for this block:

        self.name = name
        self.re_start = re_start
        self.re_end = re_end

        # Text lines that we have matched as start/end text for this block:

        self.start_hits = []
        self.end_hits = []

    def starts_on(self, rline):
        """
        Called on a report text line found while searching for a possible new
        block start.
        """

        p = re.match(self.re_start, rline)
        if p:
            self.start_hits.append(rline)
        return p

    def ends_on(self, rline):
        """
        Called on a report text line found while searching for a possible end
        of the current block. Return the match operation result.
        """

        p = re.match(self.re_end, rline)
        if p:
            self.end_hits.append(rline)
        return p

    def check(self):
        """
        Once we're done reading the entire report, sanity check what we found
        for this block. Raise a test failure.
        """

        n_starts = len(self.start_hits)
        n_ends = len(self.end_hits)

        thistest.fail_if(
            n_starts != n_ends,
            "(%s report section): %d starts != %d ends"
            % (self.name, n_starts, n_ends),
        )

    def value(self, count):
        return 0 if count == "No" else int(count)


# ---------------------------
# -- Rchapter and Rsection --
# ---------------------------

# Specialized report "sections" and "chapters", as contents blocks with a
# numbering at the start of the header title, possibly preceded by decorative
# characters.


def re_block_number(pre, ndigits):
    return pre + r"[0-9]\." * ndigits + " "


class Rchapter(Rblock):
    # Chapter titles are characterized by a single digit number preceded by
    # two decorative characters, for example:
    #
    # == 2. COVERAGE VIOLATIONS ==

    def __init__(self, re_start, re_end):
        Rblock.__init__(
            self,
            name=re_start,
            re_start=re_block_number(pre=".. ", ndigits=1) + re_start,
            re_end=re_end,
        )


class Rsection(Rblock):
    # Section titles are characterized by a single digit number preceded by
    # two decorative characters, for example:
    #
    # 2.1. STATEMENT COVERAGE VIOLATIONS

    def __init__(self, re_start, re_end):
        Rblock.__init__(
            self,
            name=re_start,
            re_start=re_block_number(pre="", ndigits=2) + re_start,
            re_end=re_end,
        )


# --------------------------------
# -- Nblock, Nsection, Nchapter --
# --------------------------------
#
# eNote blocks (violations, exempted regions, non-coverable items, other
# messages), which all contain a list of emitted notes + a counter of the
# number of such notes at the end. Some are sections, some are chapters.


class Nblock(Rblock):
    def __init__(self, re_notes):
        self.re_notes = re_notes

        # Remember the set of notes that we found in this block, so
        # we can compare the count with what the end of block summary
        # line advertises.

        self.enotes = []

    def try_parse_enote(self, rline):
        dline = Rdiagline_from(rline)

        # If no match at all, punt.

        if not dline:
            return None

        # Otherwise, we construct the Enote object incrementally, as we need
        # to sort out the note kind and separation tag from the diagnostic
        # text

        enote = Enote(
            segment=dline.sloc.section,
            source=dline.sloc.filename,
            kind=None,
            stag=None,
        )

        # Fetch and remove a possible separation tag from the diagnostic
        # text. Removal is useful to facilitate matching of other parts, hence
        # attempted first.

        def __stag_replacement_for(m):
            # Side effect on caller here
            enote.stag = Stag_from(m.group(1), True)
            return ""

        this_diag = re.sub(
            pattern=r" \(from (.*)\)",
            repl=__stag_replacement_for,
            string=dline.diag,
        )

        # Then determine the note kind from the remaining contents

        enote.kind = self.nkind_for(this_diag)

        return self.check_enote(this_diag, enote)

    def report_unexpected_enote(self, rline):
        thistest.failed(
            "(%s report section) '%s' does not"
            " correspond to any expected violation note"
            % (self.name, rline.rstrip("\n"))
        )

    def check_enote(self, rline, enote):
        if enote.kind is None:
            self.report_unexpected_enote(rline)
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
        thistest.fail_if(
            count != self.ecount,
            "(%s report section) recognized %d notes != summary (%d)\n"
            % (self.name, self.ecount, count),
        )

    def ends_on(self, rline):
        p = Rblock.ends_on(self, rline)
        if p:
            self.validate_ecount(count=self.value(p.group(1)))
        return p

    def re_summary(self):
        """
        Regexp matching the string that we expect to find in the analysis
        summary line for this block, with a group holding the note counter.
        """

        return self.re_end

    def nkind_for(self, rline):
        for key in self.re_notes:
            if re.match(key, rline):
                return self.re_notes[key]
        return None


class Nsection(Nblock, Rsection):
    def __init__(self, re_start, re_end, re_notes):
        Rsection.__init__(self, re_start=re_start, re_end=re_end)
        Nblock.__init__(self, re_notes=re_notes)


class Nchapter(Nblock, Rchapter):
    def __init__(self, re_start, re_end, re_notes):
        Rchapter.__init__(self, re_start=re_start, re_end=re_end)
        Nblock.__init__(self, re_notes=re_notes)


# VIOsection, OERsection, XREchapter, NCIchapter, SMRchapter, DCRChapter
#
# Leaf specializations, a set of which will be instantiated for report
# processing.


class VIOsection(Nsection):
    """Coverage Violations section."""

    def __init__(self, re_start, re_notes):
        Nsection.__init__(
            self,
            re_start=re_start,
            re_end=r"(No|\d+) violation[s]*\.$",
            re_notes=re_notes,
        )

    def re_summary(self):
        return r"(No|\d+).* %s violation[s]*\.$" % self.name.split()[0]


class OERsection(Nsection):
    """Other errors section."""

    def __init__(self, re_start):
        Nsection.__init__(
            self,
            re_start=re_start,
            re_end=r"(No|\d+) message[s]*\.$",
            re_notes=None,
        )

    def nkind_for(self, rline):
        # Messages in this section are always unexpected and should trigger
        # test failure. Just tell we don't know how to bind them on any sort
        # of expected note kind, and let the generic engine do the rest.

        return None


class XREchapter(Nchapter):
    """Exemptions Regions chapter."""

    def __init__(self, re_start, re_notes, exempted_notes):
        # The list of notes acceptable for an exempted violation
        self.exempted_notes = exempted_notes

        # The number of exempted violations found
        self.ecount_exempted = 0

        re_end = (
            r"(No|\d+) exempted region[s]*, "
            r"(No|\d+) exempted violation[s]*\.$"
        )

        Nchapter.__init__(
            self, re_start=re_start, re_end=re_end, re_notes=re_notes
        )

    def __check_summary_count(self, sec, sum_count, sec_count):
        thistest.fail_if(
            sum_count != sec_count,
            "summary count %d != section count %d for %s"
            % (sum_count, sec_count, sec.name),
        )
        thistest.fail_if(
            len(sec.start_hits) != 1,
            "summary found for section starts != 1 (%s)" % sec.name,
        )

    def check_exempted_summary(self, sec, p):
        # The emitted notes of exempted violations are logged in self.enotes
        # to be checked for discharging expected notes later on. However
        # we still need to check the number of notes belonging to the exempted
        # region itself and the number of notes for exempted violations
        # separately.
        ecount = len(self.enotes) - self.ecount_exempted

        self.__check_summary_count(sec, self.value(p.group(1)), ecount)

        self.__check_summary_count(
            sec, self.value(p.group(2)), self.ecount_exempted
        )

    def __validate(self, actual_count, count, element_counted):
        thistest.fail_if(
            actual_count != count,
            "(%s report section) recognized %d notes != summary (%d)"
            "for (%s)\n" % (self.name, actual_count, count, element_counted),
        )

    def ends_on(self, rline):
        p = Rblock.ends_on(self, rline)
        if p:
            # Validate the number of exempted regions
            self.__validate(
                len(self.enotes) - self.ecount_exempted,
                self.value(p.group(1)),
                "exempted regions",
            )

            # Validate the total number of exempted violations
            self.__validate(
                self.ecount_exempted,
                self.value(p.group(2)),
                "exempted violations",
            )

        return p

    def check_enote(self, rline, enote):
        if enote.kind is None:
            self.report_unexpected_enote(rline)
            return None

        # Check if the note is that of an exempted violation in the region. If
        # so, increment the exempted violations counter.
        if enote.kind in self.exempted_notes.values():
            self.ecount_exempted += 1

        return enote


class DCRchapter(Nsection):
    """Disabled Coverage Regions section."""

    def __init__(self, re_start):
        Nchapter.__init__(
            self,
            re_start=re_start,
            re_end=r"(\d+) region[s]* with disabled coverage\.$",
            re_notes={".*": dBlock},
        )

    def re_summary(self):
        return r"(\d+) region[s]* with disabled coverage\.$"


class NCIchapter(Nchapter):
    """Non-coverable Items chapter."""

    def __init__(self, re_start, re_notes):
        Nchapter.__init__(
            self,
            re_start=re_start,
            re_end=r"(No|\d+) exclusion[s]*\.$",
            re_notes=re_notes,
        )

    def re_summary(self):
        return r"(No|\d+) coverage exclusion[s]*\.$"


class UCIchapter(Nchapter):
    """Undetermined Coverage Items chapter."""

    def __init__(self, re_start, re_notes):
        Nchapter.__init__(
            self,
            re_start=re_start,
            re_end=r"(\d+) undetermined coverage item[s]*\.$",
            re_notes=re_notes,
        )

    def re_summary(self):
        return r"(\d+) undetermined coverage item[s]*\.$"


class SMRchapter(Rchapter):
    """Analysis Summary chapter."""

    def __init__(self, re_start, skeys):
        Rchapter.__init__(
            self, re_start=re_start, re_end=".. END OF REPORT ..$"
        )
        self.skeys = skeys
        self.checked = dict.fromkeys(skeys, False)

    def try_match(self, sec, rline):
        p = re.match(self.skeys[sec], rline)
        if p:
            # If the check is done on the exempted regions chapter then the
            # summary verification has to be done on the number of regions and
            # the number of violations

            if sec.name == "EXEMPTED REGIONS":
                sec.check_exempted_summary(sec, p)
            else:
                sum_count = self.value(p.group(1))
                sec_count = sec.ecount
                thistest.fail_if(
                    sum_count != sec_count,
                    "summary count %d != section count %d for %s"
                    % (sum_count, sec_count, sec.name),
                )
                thistest.fail_if(
                    len(sec.start_hits) != 1,
                    "summary found for section starts != 1 (%s)" % sec.name,
                )
            self.checked[sec] = True

    def try_parse(self, rline):
        for sec in self.skeys:
            if not self.checked[sec]:
                self.try_match(sec, rline)
        return None

    def check(self):
        Rchapter.check(self)

        for sec in self.skeys:
            thistest.fail_if(
                len(sec.start_hits) > 0 and not self.checked[sec],
                "summary count check missing for section %s" % sec.name,
            )


# At this point, we have the class hierarchy involved in modeling the
# report parts of interest. Here is a general sketch:
#
#                    Rblock
#                    | | |
#       o------------o | o------------o
#       |              |              |
#    Rsection        Nblock        Rchapter
#       |            |   |          |   |
#       o------o-----o   o----o-----o   |
#              |              |         |
#           Nsection       Nchapter     |
#              |              |         |
#          VIOsection     XMPchapter  SMRchapter
#          OERsection     NCIchapter
#                         UCIchapter
#
# We now add grab bags specializations aimed at catching
# unexpected blocks:


class Ublock(Rblock):
    def check(self):
        thistest.fail_if(
            len(self.start_hits) > 0,
            "Unexpected headers caught by %s:\n%s"
            % (self.__class__.__name__, "".join(self.start_hits)),
        )


class Uchapter(Ublock, Rchapter):
    def __init__(self):
        Rchapter.__init__(self, re_start=".", re_end=".")


class Usection(Ublock, Rsection):
    def __init__(self):
        Rsection.__init__(self, re_start=".", re_end=".")


# And for blocks we expect but of uninteresting contents:
#
# Just claim immediate end. Everything will be ignored until
# the next section or block starter.


class Tchapter(Rchapter):
    def __init__(self, re_start):
        Rchapter.__init__(self, re_start=re_start, re_end=".")


# Now ready to materialize and wrapup the whole set of blocks needed to
# process a report:


class RblockSet:
    def __init__(self):
        # We need a list of all the blocks to drive the report parsing
        # process, and a list of all the note blocks to setup the analysis
        # summary checker:

        self.noteblocks = []
        self.allblocks = []

        # Violation sections

        stmt_notes = {
            "statement not executed": sNoCov,
            "multiple statements on line": sPartCov,
        }

        self.noteblocks.append(
            VIOsection(re_start="STMT COVERAGE", re_notes=stmt_notes)
        )

        dc_notes = {
            "decision outcome FALSE never": dfNoCov,
            "decision outcome TRUE never": dtNoCov,
            "decision never evaluated": dNoCov,
            "decision not exercised in both directions": dPartCov,
        }
        self.noteblocks.append(
            VIOsection(re_start="DECISION COVERAGE", re_notes=dc_notes)
        )

        mcdc_notes = {
            "decision outcome FALSE never": efNoCov,
            "decision outcome TRUE never": etNoCov,
            "decision never evaluated": eNoCov,
            "decision not exercised in both directions": ePartCov,
            "condition has no independent influence pair": cPartCov,
        }
        self.noteblocks.append(
            VIOsection(re_start="UC_MCDC COVERAGE", re_notes=mcdc_notes)
        )
        self.noteblocks.append(
            VIOsection(re_start="MCDC COVERAGE", re_notes=mcdc_notes)
        )

        # Assertion coverage

        atc_notes = {
            "contract expression outcome TRUE never": atNoCov,
            "contract expression never evaluated": aNoCov,
        }
        self.noteblocks.append(
            VIOsection(re_start="ATC COVERAGE", re_notes=atc_notes)
        )

        atcc_notes = {
            "condition was never evaluated during an evaluation of the "
            "decision to True": acPartCov
        }
        self.noteblocks.append(
            VIOsection(re_start="ATCC COVERAGE", re_notes=atcc_notes)
        )

        fun_call_notes = {
            "function not executed": fNoCov,
            "call not executed": cNoCov,
        }
        self.noteblocks.append(
            VIOsection(re_start="FUN_CALL COVERAGE", re_notes=fun_call_notes)
        )

        # Non coverable items

        nc_notes = {
            "statement has no object code": sNotCoverable,
            "decision is always TRUE": dtAlways,
            "decision is always FALSE": dfAlways,
            "decision has no object code": dNotCoverable,
        }
        self.noteblocks.append(
            NCIchapter(re_start="NON COVERABLE ITEMS", re_notes=nc_notes)
        )

        # Undetermined coverage items

        ni_notes = {
            "statement was not instrumented": sUndetCov,
            "decision was not instrumented"
            " for decision coverage": dUndetCov,
            "decision was not instrumented for MC/DC coverage": eUndetCov,
            "function was not instrumented": fUndetCov,
            "call was not instrumented": cUndetCov,
        }

        self.noteblocks.append(
            UCIchapter(
                re_start="UNDETERMINED COVERAGE ITEMS", re_notes=ni_notes
            )
        )

        # Exempted regions

        xr_notes = {
            "0 exempted violation": xBlock0,
            r"[1-9]\d* exempted violation": xBlock1,
            r"\d+ exempted violations?; [1-9]\d+ exempted undetermined "
            r"coverage items?": xBlock2,
        }

        # Exempted violations

        Xr_notes = {
            "statement not executed": XsNoCov,
            "multiple statements on line": XsPartCov,
            "statement has no object code": XsNotCoverable,
            "statement was not instrumented": XsUndetCov,
            "decision outcome FALSE never": XofNoCov,
            "decision outcome TRUE never": XotNoCov,
            "decision outcome never evaluated": XoNoCov,
            "decision not exercised in both directions": XoPartCov,
            "condition has no independent influence pair": XcPartCov,
        }

        self.noteblocks.append(
            XREchapter(
                re_start="EXEMPTED REGIONS",
                re_notes=xr_notes | Xr_notes,
                exempted_notes=Xr_notes,
            )
        )

        # Other note blocks

        self.noteblocks.append(OERsection(re_start="OTHER ERRORS"))

        self.noteblocks.append(
            DCRchapter(
                re_start="COVERAGE DISABLED REGIONS",
            )
        )

        # We're done with the note blocks at this point

        self.allblocks.extend(self.noteblocks)

        # Analysis Summary chapter

        self.allblocks.append(
            SMRchapter(
                re_start="ANALYSIS SUMMARY",
                skeys={s: s.re_summary() for s in self.noteblocks},
            )
        )

        # Transparent chapters

        self.allblocks.append(Tchapter(re_start="ASSESSMENT CONTEXT"))
        self.allblocks.append(
            Tchapter(re_start="(NON-EXEMPTED )?COVERAGE VIOLATIONS")
        )

        # Unexpected chapter/section catchers

        self.allblocks.append(Usection())
        self.allblocks.append(Uchapter())

    def starts_with(self, rline):
        for rs in self.allblocks:
            if rs.starts_on(rline):
                return rs
        return None

    def check(self):
        for rs in self.allblocks:
            rs.check()


class RnotesExpander:
    """Produce list of Enote instances found in a "report" output."""

    def to_enotes(self, report):
        # We need to ignore everything not in the report sections
        # of interest, so until we know we're in ...

        self.rset = RblockSet()
        self.rs = None

        self.report = report
        Tfile(filename=self.report, process=self.process_tline)

        self.rset.check()

    def register(self, enote):
        source = enote.source
        if source not in self.ernotes:
            self.ernotes[source] = KnoteDict(erNoteKinds)
        self.ernotes[source].register(enote)

    def process_tline(self, tline):
        rline = tline.text

        # Check if we are getting in a section of interest. If so, register
        # that and get to next line.

        rs = self.rset.starts_with(rline)
        if rs:
            self.rs = rs
            return None

        # Check if we are getting out of the current section of interest ...

        if self.rs and self.rs.ends_on(rline):
            self.rs = None

        # Skip this line if we're out of any section of interest

        if self.rs is None:
            return None

        enote = self.rs.try_parse(rline)

        # Some sections produce enotes, some don't (e.g. analysis summary).
        # An error is issued by the section processing if it should find one
        # but couldn't.

        if enote:
            self.register(enote)

        return enote

    def __init__(self, report):
        # xcov --annotate=report produces a single report featuring a list of
        # indications for slocs in all the units.

        self.ernotes = {}
        self.to_enotes(report)
