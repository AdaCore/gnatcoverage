# ****************************************************************************
# **                TESTSUITE QUALIFICATION DATA FACILITIES                 **
# ****************************************************************************

# This module exposes the testsuite qualification data management facilities,
# aimed at the production of a test-results report at the end of the testsuite
# execution.
#
# Testcases dump Qdata instances when they execute. The testsuite driver
# retrieves and accumulates them on the fly, then produces a report from the
# complete set at the end.

# ****************************************************************************

import os, pickle

from SCOV.internals.cnotes import *

QDATA_PKFILE = "tc.pkl"

# -------------
# -- qdaf_in --
# -------------

def qdaf_in(dir):
    """Filename for qualification data to be pickled in DIR"""
    return os.path.join (dir, QDATA_PKFILE)

# ================================================================
# == Qualification Data classes, filled and dumped by testcases ==
# ================================================================

# We expect one Qdata instance per qualification testcase, which hosts a
# sequence of entries; typically one per SCOV_helper execution (that is, per
# driver + consolidation spec)

class QDentry:

    def __init__(self, eid, xrnotes):
        self.eid = eid           # entry id
        self.xrnotes = xrnotes   # expected report notes, KnoteDict per source

class Qdata:
    def __init__(self, tcid):
        self.entries = []
        self.errcount = None
        self.tcid = tcid

    def register (self, ob):
        self.entries.append (ob)

    def flush(self, errcount):
        self.errcount = errcount
        with open(qdaf_in("."), "w") as fd:
            pickle.dump(self, fd)

# ========================================================================
# == QDregistry - Qualification Data repository for the toplevel driver ==
# ========================================================================

# list, for each testcase, of:
#
#  qdata  with .tcid
#              .entries = [ (.eid, .xrnotes), (.eid, .xrnotes), ... ]
#                                      |
# -------------------------------------+--------
#    source ->   kind  -> list of expected notes
#       v         vv             vvvvv
# { "bla.adb": { sNoCov : [ Xnote, Xnote, ... ],
#                dTNoCov: [ Xnote, ... ]
#                ...
#              }
#    ...
# }

class QDregistry:

    def __init__(self):

        # The full qualification data for this report; sequence of testcase
        # Qdata instances.

        self.qdl = []

    def check_qdata (self, qdaf):
        """Once we're done executing a testcase, check if
        there is a qdata object pickled in QDAF and retrieve it if so."""

        if not os.path.exists(qdaf):
            return

        with open (qdaf) as fd:
            self.qdl.append (pickle.load (fd))

# ==============
# == QDreport ==
# ==============

nov, scv, dcv, ccv, xbv, sta = range (6)

columns = {
    nov : "nov",
    scv : "scv",
    dcv : "dcv",
    ccv : "ccv",
    xbv : "xbv",
    sta : "status" }

cnt_columns = [nov, scv, dcv, ccv, xbv]

column_for = {
    r0      : nov,
    xBlock0 : nov,

    sNoCov   : scv, sPartCov : scv,
    dtNoCov  : dcv, dfNoCov  : dcv, dPartCov : dcv, dNoCov : dcv,
    cPartCov : ccv,

    xBlock1 : xbv
}

class ColCounts:
    def __init__(self):
        self.total = 0
        self.satisfied = 0

    def __str__(self):
        if self.satisfied != self.total:
            return "%3d/%-3d" % (self.satisfied, self.total)
        else:
            return "%-7d" % self.total

class ColErr:
    def __init__(self, errcount):
        self.errcount = errcount

    def __str__(self):
        return "passed " if self.errcount == 0 else "FAILED "

class RSTfile:
    def __init__(self, filename):
        self.fd = None
        self.open (filename)

    def open(self, filename):
        self.fd = open ("qreport/source/" + filename, 'w')

    def write(self, text, postfix = '\n'):
        self.fd.write (text+postfix)

    def close(self):
        self.fd.close()

class QDreport:

    def __init__(self, qdreg):
        self.qdl = qdreg.qdl

        self.rstf = None

        self.gen_envinfo()
        self.gen_tctable()
        self.gen_tcsummary()
        self.gen_index()

    # -----------------------------
    # -- gen_tctable and helpers --
    # -----------------------------

    def count(self, rdline, note):
        rdcol = rdline[column_for[note.kind]]
        rdcol.total += 1

        if note.satisfied():
            rdcol.satisfied += 1
        else:
            print note.__dict__

    def process_qdata(self, qdata):

        rdline = {}
        [rdline.__setitem__(key, ColCounts()) for key in cnt_columns]

        rdline.__setitem__(sta, ColErr(qdata.errcount))

        [self.count (rdline, note)
         for qde in qdata.entries for src in qde.xrnotes
         for notelist in qde.xrnotes[src].itervalues()
         for note in notelist]

        self.rdata [qdata.tcid] = rdline
        self.tcid_len = max (self.tcid_len, len(qdata.tcid))

    def gen_tcline_for(self, tcid):
        rdline = self.rdata[tcid]
        tcline = " ".join (
            ["%-*s" % (self.tcid_len, tcid)]
            + [str(rdline[col]) for col in rdline])

        self.rstf.write (tcline)

    def gen_tctable(self):

        # Width of the testcase id column on every line. Start with a min to
        # have room for at least the column title
        self.tcid_len = 8

        # Report data from which we'll generate the lines of text, after
        # aggregating counters for all the entries for each testcase
        self.rdata = {}

        # Do compute the column values now. This is not their string
        # representation yet

        [self.process_qdata(qd) for qd in self.qdl]

        # Now compute and write out the string representations, wrapped
        # in a simple REST table construct. Something like
        #
        # =========== ======== ======== ... =======
        # testcase    nov      scv          ok?
        # =========== ======== ======== ... =======
        # Qualif/bla  3        1            :-)
        # ...
        # =========== ======== ======== ... =======


        sepl = ("=" * self.tcid_len
                + (" " + "=" * 7) * len(columns))

        headl = " ".join (
            ["%-*s" % (self.tcid_len, "testcase")]
            + ["%-7s" % columns[col] for col in columns])

        self.rstf = RSTfile ("tctable.rst")

        self.rstf.write (sepl)
        self.rstf.write (headl)
        self.rstf.write (sepl)

        [self.gen_tcline_for(qd.tcid) for qd in self.qdl]

        self.rstf.write (sepl)

        self.rstf.close()

    def gen_tcsummary(self):
        self.rstf = RSTfile ("tcsummary.rst")
        self.rstf.close()

    def gen_index(self):
        self.rstf = RSTfile ("index.rst")
        self.rstf.close()

    def gen_envinfo(self):
        self.rstf = RSTfile ("envinfo.rst")
        self.rstf.close()


