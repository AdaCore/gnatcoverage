# ****************************************************************************
# **                      EMITTED REPORT NOTE EXPANDERS                     **
# ****************************************************************************

# Expose the RnotesExpander class which constructs a { source -> KnoteDict }
# dictionary of emitted Line Notes from a provided =report outputs.

# ***************************************************************************

import re

from SUITE.context import thistest

from gnatpython.fileutils import ls

from . cnotes import xBlock0, xBlock1, sNoCov, sPartCov
from . cnotes import dfNoCov, dtNoCov, dNoCov, dPartCov
from . cnotes import efNoCov, etNoCov, eNoCov, ePartCov, cPartCov
from . cnotes import Enote, KnoteDict, erNoteKinds

from . segments import Sloc, Sloc_from_match

from . tfiles import Tfile
from . stags import Stag_from

# =========================================
# == Report sections asbtraction classes ==
# =========================================

# Helpers for the RnotesExpander main class, to let us control when looking
# for indication patterns and check that each appears in the section where we
# expect it.

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

# ====================
# == RnotesExpander ==
# ====================

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
