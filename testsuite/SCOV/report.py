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

        # Expected predecessor pattern. The last match of these
        # will need to happen before the first match of self.

        self.pre = pre

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
        thistest.fail_if (
            len (self.matches) < self.nexpected,
            'too few matches of pattern "%s"' % self.pattern)
        thistest.fail_if (
            len (self.matches) > self.nexpected,
            'too many matches of pattern "%s"' % self.pattern)

        # Check for expected ordering of pieces

        thistest.fail_if (
            self.pre and
            (self.pre.__last_match().lno > self.__first_match().lno),
            'first match for "%s" too early wrt predecessor "%s"' %
            (self.pattern, self.pre.pattern if self.pre else "err"))

# ==========================
# == Whole report checker ==
# ==========================

class ReportChecker:

    def __process_line(self,tline):
        [rpe.check_match (tline) for rpe in self.rpElements]

    def __setup_expectations(self, ntraces):

        rpStart  = Piece (
            pattern="COVERAGE REPORT", pre=None)

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
            pattern="trace files:", pre=covLevel)

        trFile = Piece (
            pattern="\.trace", pre=trHeader, nexpected=ntraces)
        trPgm = Piece (
            pattern="program:", pre=None, nexpected=ntraces)
        trDate = Piece (
            pattern="date:", pre=None, nexpected=ntraces)
        trTag = Piece (
            pattern="tag:", pre=None, nexpected=ntraces)

        vioHeader = Piece (
            pattern="NON-EXEMPTED VIOLATIONS", pre=trTag)
        vioCount = Piece (
            pattern="([0-9]+|No) violation", pre=vioHeader)

        xmrHeader = Piece (
            pattern="EXEMPTED REGIONS", pre=vioCount)
        xmrCount = Piece (
            pattern="([0-9]+|No) exempted region", pre=xmrHeader)

        rpEnd    = Piece (
            pattern="END OF REPORT", pre=xmrCount)

        self.rpElements = [
            rpStart,
            ctxHeader, runStamp, verNumber,
            cmdLine1, cmdLine2,
            covLevel, trHeader, trFile, trPgm, trDate, trTag,
            vioHeader, vioCount, xmrHeader, xmrCount,
            rpEnd]

    def __init__(self, subdir, ntraces):
        self.__setup_expectations(ntraces)
        self.report = Tfile ("tmp_%s/test.rep" % subdir, self.__process_line)

    def run (self):
        [rpe.check () for rpe in self.rpElements]

