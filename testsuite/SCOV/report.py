# ***************************************************************************
# **                           REPORT format checker                       **
# ***************************************************************************

# This module exposes the ReportChecker class.

# Dev in progress. Still pretty crude ...

# ***************************************************************************

import re, os.path

from SUITE.tutils import thistest, frame
from SUITE.cutils import FatalError, no_ext
from SUITE.control import XCOV

from SCOV.internals.cnotes import xNoteKinds

from internals.tfiles import *

# What the whole report checker should do
MATCH_NEXT_PIECE, MATCH_NEXT_LINE = range (2)

# ===========================================================
# == Single pattern of text expected somewhere in a report ==
# ===========================================================

class Piece:
    def __init__(self, pattern, pre, nexpected=1):

        # regexp pattern to match over report lines

        self.pattern = pattern

        # Whether this is a "bounded" Piece, which should stop hitting matches
        # at some point.

        self.bounded = nexpected < 0

        # We use this to deal with cases of identical patterns that can
        # legitimately appear several times at different places and that we
        # wish to match individually. Consider for example
        #
        #    STMT VIOLATIONS
        #    ...
        #    2 violations
        #
        #    DECISION VIOLATIONS
        #    ...
        #    3 violations
        #
        # and we want to recognize the two "\d+ violations" patterns as
        # distinct instances, each hit once with its correct line number.

        # How many times (lines) we expect to match and set of
        # tline instances that did actually match

        self.nexpected = abs(nexpected)
        self.matches = []

        # Expected predecessor and successor patterns.

        self.pre = pre

        # For nexpected > 0 the last match of pre will need to happen before
        # the first match of self

    def full (self):
        return self.bounded and len (self.matches) == self.nexpected

    # Called for self on every report line

    def check_match(self,tline):

        # If this is a bounded Piece that has hit all it's expected matches
        # already, just skip it: don't record an extra match and keep looking
        # for other Piece candidates for that text

        if self.full():
            return MATCH_NEXT_PIECE

        # Otherwise, check if the provided line matches this Piece. If it
        # does, record and check if that fills this possibly bounded Piece.
        # If it does, consider that line the "property" of this Piece and
        # don't try to match it against following Pieces. The other Pieces
        # that would have matched this (as well) will need to be hit by a
        # different line.

        if re.search (self.pattern, tline.text):
            self.matches.append (tline)
            if self.full():
                return MATCH_NEXT_LINE

        return MATCH_NEXT_PIECE

    # Check expectations once we're done going through all the report lines

    def __first_match(self):
        return self.matches[0]

    def __last_match(self):
        return self.matches[-1]


    def check(self):

        nmatches = len (self.matches)

        0 and thistest.log (
            "--\nChecking %s:\n" % str(self)
            + "pattern = '%s', nexpected = %d, nmatches = %d\n"
               % (self.pattern, self.nexpected, nmatches)
            + "pre = %s" % str (self.pre)
            )


        # Check that we have the number of expected matches

        thistest.fail_if (
            nmatches != self.nexpected,
            '%d matches of pattern "%s", != expected %d' % (
                nmatches, self.pattern, self.nexpected)
            )

        # If we expected matches, have some, and have an ordering
        # constraint specified, check it

        if self.nexpected > 0 and nmatches != 0 and self.pre:
            last_pre = self.pre.__last_match().lno
            first_self = self.__first_match().lno
            thistest.fail_if (
                last_pre > first_self,
                'first match for "%s" (%d) too early wrt predecessor "%s" (%d)'
                % (self.pattern, first_self,  self.pre.pattern, last_pre)
                )

# ==========================
# == Whole report checker ==
# ==========================

# All the possible per-criterion sections, ordered as they
# should be in the report

all_crit = ["STMT", "DECISION", "MCDC"]

# Those that we expect for each xcov --level

crit_for = {
    "stmt":          ["STMT"],
    "stmt+decision": ["STMT", "DECISION"],
    "stmt+mcdc":     ["STMT", "DECISION", "MCDC"]
}


class ReportChecker:

    def __process_line(self,tline):

        # See what Piece(s) matches TLINE, stopping on request
        for rpe in self.rpElements:
            if rpe.check_match (tline) == MATCH_NEXT_LINE:
                return

    def __register(self, rpieces):
        self.rpElements.extend (rpieces)

    def __setup_expectations(self, ntraces, xcovlevel, xregions):

        self.rpElements = []

        # Track the last Piece with nexpected > 0
        pre = None

        # REPORT START

        rpStart  = Piece (
            pattern="COVERAGE REPORT", pre=None)

        self.__register (rpieces = [rpStart])

        # ASSESSMENT CONTEXT

        ctxHeader = Piece (
            pattern="ASSESSMENT CONTEXT", pre=rpStart)

        runStamp = Piece (
            pattern="Date and time of execution:", pre=ctxHeader)
        verNumber =  Piece (
            pattern="Tool version:", pre=runStamp)

        cmdLine1 = Piece (
            pattern="Command line:", pre=verNumber)
        cmdLine2 = Piece (
            pattern="%s(\.exe)? coverage" % XCOV, pre=cmdLine1)

        covLevel = Piece (
            pattern="Coverage level: stmt(\+(decision|mcdc))?", pre=cmdLine2)

        trHeader = Piece (
            pattern="Trace files:", pre=covLevel)

        trFile = Piece (
            pattern="\.trace", pre=trHeader, nexpected=ntraces)
        trPgm = Piece (
            pattern="program *:", pre=None, nexpected=ntraces)
        trDate = Piece (
            pattern="date *:", pre=None, nexpected=ntraces)
        trTag = Piece (
            pattern="tag *:", pre=None, nexpected=ntraces)

        self.__register (
            rpieces = [ctxHeader, runStamp, verNumber,
                       cmdLine1, cmdLine2,
                       covLevel, trHeader, trFile, trPgm, trDate, trTag]
            )

        # NON-EXEMPTED VIOLATIONS

        vioHeader = Piece (
            pattern = ("\d+. %sCOVERAGE VIOLATIONS"
                       % ("NON-EXEMPTED " if xregions else "")),
            pre = trTag)

        self.__register (rpieces = [vioHeader])

        # We want to check that the expected sections are there, and that the
        # unexpected sections are not there. We create Pieces with nexpected
        # == 0 for this purpose, and will check that nmatches == 0 as well.

        pre = vioHeader

        for crit in all_crit:

            # How many instances of a section we expect for a criterion
            # depends on the xcov --level argument

            nexpected = 1 if crit in crit_for [xcovlevel] else 0

            vsHeader = Piece (
                pattern   ="%s COVERAGE" % crit,
                nexpected = nexpected,
                pre = pre if nexpected else None)

            self.__register (rpieces = [vsHeader])

            # If we do expect a section, add a pattern for the violation
            # counter and update the current pre chain reference. Unexpected
            # sections should be left out of that chain.

            if nexpected > 0:
                vsCount = Piece (
                    pattern   = "([0-9]+|No) violation",
                    nexpected = -1,
                    pre = vsHeader)

                self.__register (rpieces = [vsCount])

                pre = vsCount

        # EXEMPTED REGIONS

        if xregions:
            xmrHeader = Piece (
                pattern = "EXEMPTED REGIONS", pre=pre)
            xmrCount = Piece (
                pattern = "([0-9]+|No) exempted region", nexpected=-1,
                pre = xmrHeader)

            self.__register (rpieces = [xmrHeader, xmrCount])
            pre = xmrCount

        # ANALYSIS SUMMARY

        sumHeader = Piece (
            pattern = "ANALYSIS SUMMARY",
            pre = pre)

        self.__register (rpieces = [sumHeader])
        pre = sumHeader

        for crit in crit_for [xcovlevel]:
            sumLine = Piece (
                pattern = ("([0-9]+|No) %s%s violation" %
                           ("non-exempted " if xregions else "", crit)),
                pre = pre)
            self.__register (rpieces = [sumLine])
            pre = sumLine

        if xregions:
            sumLine = Piece (
                pattern = "([0-9]+|No) exempted region",
                nexpected = -1,
                pre = pre)
            self.__register (rpieces = [sumLine])
            pre = sumLine

        # END OF REPORT

        rpEnd    = Piece (
            pattern="END OF REPORT", pre=pre)
        self.__register (rpieces = [rpEnd])

    def __process_one_test (self, qde):

        frame (text = ("report check for xfile = %s\n" % qde.xfile
                       + "drivers = %s" % str(qde.drivers)),
               char = '~').display()

        # Count the number of expected exemption regions

        xregions = 0
        for source_xrn in qde.xrnotes.values():
            for kind in xNoteKinds:
                xregions += len (source_xrn[kind])

        # We're looking at the last report produced, with the last
        # applicable xcov-level

        self.__setup_expectations(
            ntraces   = len(qde.drivers),
            xcovlevel = self.tc.xcovlevels[-1],
            xregions  = xregions
            )

        self.report = Tfile (
            "tmp_%s/test.rep" % no_ext(os.path.basename(qde.xfile)),
            self.__process_line)

        [rpe.check () for rpe in self.rpElements]

    def __init__(self, tc):
        self.tc = tc

    def run (self):
        [self.__process_one_test (qde=qde) for qde in self.tc.qdata.entries]

