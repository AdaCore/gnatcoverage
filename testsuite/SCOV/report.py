# ***************************************************************************
# **                           REPORT format checker                       **
# ***************************************************************************

# This module exposes the ReportChecker class.

# Dev in progress. Still pretty crude ...

# ***************************************************************************

import re

from SUITE.tutils import thistest
from SUITE.cutils import FatalError

from internals.tfiles import *

# ===========================================================
# == Single pattern of text expected somewhere in a report ==
# ===========================================================

class Piece:
    def __init__(self, pattern, pre, nexpected=1):

        # regexp pattern to match over report lines

        self.pattern = pattern

        # How many times (lines) we expect to match and set of
        # tline instances that did actually match

        self.nexpected = nexpected
        self.matches = []

        # Expected predecessor and successor patterns.

        self.pre = pre

        # For nexpected > 0 the last match of pre will need to happen before
        # the first match of self

    # Called for self on every report line

    def check_match(self,tline):
        if re.search (self.pattern, tline.text):
            self.matches.append (tline)

    # Check expectations once we're done going through all the report lines

    def __first_match(self):
        return self.matches[0]

    def __last_match(self):
        return self.matches[-1]


    def check(self):

        # Check for presence of expected pieces

        thistest.stop_if (
            len (self.matches) == 0,
            FatalError('no occurrence of pattern "%s"' % self.pattern))

        nmatches = len (self.matches)

        if self.nexpected > 0:
            thistest.fail_if (
                nmatches != self.nexpected,
                '%d matches of pattern "%s", != expected %d' % (
                    nmatches, self.pattern, self.nexpected)
                )

            thistest.fail_if (
                self.pre and
                (self.pre.__last_match().lno > self.__first_match().lno),
                'first match for "%s" too early wrt predecessor "%s"' %
                (self.pattern, self.pre.pattern if self.pre else "err"))

        else:
            thistest.fail_if (
                nmatches < abs(self.nexpected),
                '%d matches of pattern "%s", < expected %d min' % (
                    nmatches, self.pattern, abs(self.nexpected))
                )

# ==========================
# == Whole report checker ==
# ==========================

# expected per-criterion sections for each test category

crit_for = {
    "stmt":     ["STMT"],
    "decision": ["STMT", "DECISION"],
    "mcdc":     ["STMT", "DECISION", "MCDC"]
}


class ReportChecker:

    def __process_line(self,tline):
        [rpe.check_match (tline) for rpe in self.rpElements]

    def __register(self, rpieces):
        self.rpElements.extend (rpieces)

    def __setup_expectations(self, ntraces, category, xregions):

        self.rpElements = []

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
            pattern="xcov(\.exe)? coverage", pre=cmdLine1)

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
            pattern = ("%sCOVERAGE VIOLATIONS"
                       % ("NON-EXEMPTED " if xregions else "")),
            pre = trTag)

        self.__register (rpieces = [vioHeader])

        pre=vioHeader
        for crit in crit_for [category]:
            vsHeader = Piece (
                pattern="%s COVERAGE" % crit, pre=pre)
            vsCount = Piece (
                pattern="([0-9]+|No) violation", nexpected=-1, pre=vsHeader)
            self.__register (rpieces = [vsHeader, vsCount])
            pre=vsCount

        # EXEMPTED REGIONS

        if xregions:
            xmrHeader = Piece (
                pattern = "EXEMPTED REGIONS", pre=vsCount)
            xmrCount = Piece (
                pattern = "([0-9]+|No) exempted region", nexpected=-1,
                pre = xmrHeader)

            self.__register (rpieces = [xmrHeader, xmrCount])

        # ANALYSIS SUMMARY

        sumHeader = Piece (
            pattern = "ANALYSIS SUMMARY",
            pre = xmrHeader if xregions else vsHeader)

        self.__register (rpieces = [sumHeader])

        pre=sumHeader
        for crit in crit_for [category]:
            sumLine = Piece (
                pattern = ("([0-9]+|No) %s%s violation" %
                           ("non-exempted " if xregions else "", crit)),
                pre = pre)
            self.__register (rpieces = [sumLine])
            pre=sumLine

        if xregions:
            sumLine = Piece (
                pattern = "([0-9]+|No) exempted region",
                nexpected = -1,
                pre = sumLine)
            self.__register (rpieces = [sumLine])

        # END OF REPORT

        rpEnd    = Piece (
            pattern="END OF REPORT", pre=sumLine)
        self.__register (rpieces = [rpEnd])

    def __init__(self, subdir, ntraces, category, xregions):
        self.__setup_expectations(ntraces, category, xregions)
        self.report = Tfile ("tmp_%s/test.rep" % subdir, self.__process_line)

    def run (self):
        [rpe.check () for rpe in self.rpElements]

