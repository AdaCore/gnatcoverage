# ****************************************************************************
# **                TESTSUITE QUALIFICATION DATA FACILITIES                 **
# ****************************************************************************

# This module exposes the testsuite qualification data management facilities
# aimed at the production of a test-results report a testsuite execution.
#
# Testcases dump Qdata instances when they execute,
#
# The testsuite driver retrieves and accumulates them on the fly, then
# produces a report from the complete set at the end.

# ****************************************************************************

import os, pickle

from SCOV.internals.cnotes import *

QDATA_PKFILE = "tc.pkl"

# ===================================================
# == Qdata classes, filled and dumped by testcases ==
# ===================================================

# We expect one Qdata instance per qualification testcase,
# which hosts a sequence of entries, still:

class QdataEntry:

    # { sourcename -> KnoteDict } dictionaries of expected line and report
    # notes. For each testcase, we'll have one instance of this per
    # SCOV_helper execution (one per driver + one per consolidation spec)

    def __init__(self, eid, xlnotes, xrnotes):
        self.eid = eid
        self.xlnotes = xlnotes
        self.xrnotes = xrnotes

class Qdata:
    def __init__(self):
        self.entries = []

    def register (self, ob):
        self.entries.append (ob)

    def flush(self):
        with open(QDATA_PKFILE, "w") as fd:
            pickle.dump(self, fd)

# =======================================================================
# == QDR - Qualification Data Repository for the toplevel suite driver ==
# =======================================================================

class QDR:

    def __init__(self):

        # The full qualification data for this report; sequence of testcase
        # qualification data entries.

        self.qdl = []

    # ----------------------------
    # -- gen_report and helpers --
    # ----------------------------

    def check_qdata_at(self, tcdir):
        """Once we're done executing a testcase at TCDIR, check if
        there is a qdata object pickled there and retrieve it if so."""

        qdf = os.path.join (tcdir, QDATA_PKFILE)

        if not os.path.exists(qdf):
            return

        with open (qdf) as fd:
            self.qdl.append (pickle.load (fd))

    # ----------------------------
    # -- gen_report and helpers --
    # ----------------------------

    def process_qdata(self, qdata):
        pass

    def gen_report(self):
        """Once we're done executing all the testcases, generate a
        test-results qualification report from the corresponding set of qdata
        instances."""

        [self.process_qdata(qd) for qd in self.qdl]
