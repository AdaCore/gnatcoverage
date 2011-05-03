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

from REST import rest

from SCOV.internals.cnotes import *

# -------------
# -- qdaf_in --
# -------------

QDATA_PKFILE = "tc.pkl"

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

        # These are filled and dumped by the testcase execution itself:

        self.entries = []
        self.tcid = tcid

    # ------------------------
    # -- Testcase interface --
    # ------------------------

    def register(self, ob):
        self.entries.append (ob)

    def flush(self):
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

    def check_qdata(self, qdaf, status, comment):
        """Once we're done executing a testcase, check if
        there is a qdata object pickled in QDAF and retrieve it if so."""

        if not os.path.exists(qdaf):
            return

        with open (qdaf) as fd:
            qda = pickle.load (fd)

        # Fill info we have only from there, requiring access to test.opt
        # to get comments and the fail/xfail differentiation.

        qda.status = status
        qda.comment = comment

        self.qdl.append (qda)

# ======================
# == QDreport helpers ==
# ======================

class Column:
    def __init__(self, htext, legend=""):
        self.htext = htext
        self.legend = legend

class colid:
    tc = Column (
        htext="testcase", legend="Testcase")

    nov = Column (
        htext="nov", legend="No violations")

    scv = Column (
        htext="scv", legend="Statement Coverage violations")

    dcv = Column (
        htext="dcv", legend="Decision Coverage violations")

    ccv = Column (
        htext="ccv", legend="Condition Coverage violations")

    xbv = Column (
        htext="xbv", legend="Exempted violations")

    sta = Column (
        htext="status", legend="Status")

    uok = Column (
        htext="UOK", legend="UOK")

    failed = Column (
        htext="FAILED", legend="FAILED")

    ok = Column (
        htext="OK", legend="OK")

    xfail = Column (
        htext="XFAIL", legend="XFAILED")

    ovsta = Column (
        htext="OVERALL", legend="Overall status")

column_for = {

    # When counting notes, map note kinds to table columns

    r0      : colid.nov,
    xBlock0 : colid.nov,

    sNoCov   : colid.scv,
    sPartCov : colid.scv,

    dtNoCov  : colid.dcv,
    dfNoCov  : colid.dcv,
    dPartCov : colid.dcv,
    dNoCov   : colid.dcv,

    cPartCov : colid.ccv,

    xBlock1  : colid.xbv,

    # When checking status, map text passed by toplevel driver
    # to column. Note that we map UOK to the same as OK.

    'OK'     : colid.ok,
    'XFAIL'  : colid.xfail,
    'UOK'    : colid.ok,
    'FAILED' : colid.failed
}

# -------------------------------
# -- Useful sets of column ids --
# -------------------------------

# Violation counters
viocnt_columns = (colid.nov, colid.scv, colid.dcv, colid.ccv, colid.xbv)

# Status counters
stacnt_columns = (colid.ok, colid.xfail, colid.failed)


class ColCounts:
    def __init__(self):
        self.total = 0
        self.satisfied = 0

    def img(self, details=False):
        if details:
            return "%d/%d" % (self.satisfied, self.total)
        else:
            return "%d" % self.total

    def augment_by(self, other):
        self.total += other.total
        self.satisfied += other.satisfied

class ColText:
    def __init__(self, text):
        self.text = text

    def img(self, details=False):
        return "%-7s " % self.text

# =============
# == RTSfile ==
# =============

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

# ==============
# == RTStable ==
# ==============

class RSTtable:

    def __init__(self, columns, contents):

        # COLUMNS is a set of column ids and CONTENTS is a list of
        # {col -> text} dictionaries, one per table line

        self.columns = columns
        self.contents = contents

    def __dump_id(self):
        pass

    def __dump_legend(self):
        pass

    def __dump_header(self):
        sepl = " ".join (
            ["=" * self.width[col] for col in self.columns])

        headl = " ".join (
            ["%-*s" % (self.width [col], col.htext)
             for col in self.columns])

        self.rstf.write (sepl)
        self.rstf.write (headl)
        self.rstf.write (sepl)

    def __dump_centry(self, ce):
        entryl = " ".join (
            ["%-*s" % (self.width[col], ce[col]) for col in self.columns])
        self.rstf.write (entryl)

    def __dump_contents(self):
        [self.__dump_centry (ce) for ce in self.contents]

    def __dump_footer(self):
        sepl = " ".join (
            ["=" * self.width[col] for col in self.columns])
        self.rstf.write (sepl)

    def __compute_widths(self):
        self.width = {}
        [self.width.__setitem__ (col, 0) for col in self.columns]

        [self.width.__setitem__ (
                col, max (self.width[col], len(centry[col])))
         for centry in self.contents for col in self.columns]

        [self.width.__setitem__ (
                col, max (self.width[col], len(col.htext)))
         for col in self.columns]

    def dump_to(self, rstf):

        self.rstf = rstf

        self.__dump_id ()
        self.__dump_legend ()

        self.__compute_widths ()

        self.__dump_header ()
        self.__dump_contents ()
        self.__dump_footer ()

# ==============
# == QDreport ==
# ==============

class QDreport:

    def __init__(self, qdreg):
        self.qdl = qdreg.qdl

        self.rstf = None

        self.gen_envinfo()
        self.gen_tctable()
        self.gen_tcsummary()
        self.gen_index()

    # -----------------
    # -- gen_tctable --
    # -----------------

    # Compute and write out a testcase table like
    #
    # =========== ======== ======== ... =======
    # testcase    nov      scv          status
    # =========== ======== ======== ... =======
    # Qualif/bla  3        1            OK
    # ...
    # =========== ======== ======== ... =======

    def count(self, rdline, note):
        rdcol = rdline[column_for[note.kind]]
        rdcol.total += 1

        if note.satisfied():
            rdcol.satisfied += 1

    def tcdata_for(self, qd):

        # Process one qualification data item, producing a report data line
        # for one testcase

        this_tcdata = {}
        [this_tcdata.__setitem__(key, ColCounts()) for key in viocnt_columns]

        this_tcdata.__setitem__(colid.sta, ColText(qd.status))

        [self.count (this_tcdata, note)
         for qde in qd.entries for src in qde.xrnotes
         for notelist in qde.xrnotes[src].itervalues()
         for note in notelist]

        return this_tcdata

    def compute_tcdata(self):
        self.tcdata = {}
        [self.tcdata.__setitem__ (qd, self.tcdata_for (qd)) for qd in self.qdl]

    def tcdict_for(self, qd):
        r = {}

        r.__setitem__(colid.tc, "%s" % qd.tcid)

        details = qd.status != 'OK'
        [r.__setitem__(col, "%s" % self.tcdata[qd][col].img(details))
         for col in self.tcdata[qd]]

        return r

    def gen_tctable(self):

        self.compute_tcdata()

        rstf = RSTfile ("tctable.rst")

        RSTtable (
            columns = (colid.tc,) + viocnt_columns + (colid.sta,),
            contents = [self.tcdict_for(qd) for qd in self.qdl]
            ).dump_to (rstf)

        rstf.close()

    # ----------------------
    # -- gen_vcnt_summary --
    # ----------------------

    # Compute and write out a counter totals summary like
    #
    # ======== ======== ...
    # nov      scv
    # ======== ======== ...
    # 3        1
    # ======== ======== ...

    def do_vcnt(self, qd):
        [self.vcnts[key].augment_by(self.tcdata[qd][key])
         for key in viocnt_columns]

    def compute_vcnt_data(self):
        self.vcnts = {}
        [self.vcnts.__setitem__ (key, ColCounts())
         for key in viocnt_columns]
        [self.do_vcnt(qd) for qd in self.qdl]

    def vcnt_dict(self):
        r = {}
        [r.__setitem__ (col, "%s" % self.vcnts[col].img())
         for col in viocnt_columns]
        return r

    def gen_vcnt_summary(self):

        self.compute_vcnt_data ()

        RSTtable (
            columns = viocnt_columns,
            contents = [self.vcnt_dict()]
            ).dump_to (self.rstf)

    # ---------------------
    # -- gen_scnt_summary --
    # ---------------------

    # Compute and write out a status totals summary like
    #
    # ======== ======== ======= =======
    # OK       XFAIL    FAILED  OVERALL
    # ======== ======== ======= =======
    # 3        1        0       OK
    # ======== ======== ======= =======

    def do_scnt(self, qd):
        self.scnts[column_for[qd.status]] += 1

    def compute_scnt_data(self):
        self.scnts = {}
        [self.scnts.__setitem__ (key, 0) for key in stacnt_columns]

        [self.do_scnt(qd) for qd in self.qdl]

    def scnt_dict(self):
        r = {}
        [r.__setitem__ (col, "%d" % self.scnts[col])
         for col in stacnt_columns]
        r.__setitem__ (
            colid.ovsta,
            "%s" % "OK" if self.scnts[colid.failed] == 0 else "BING")
        return r

    def gen_scnt_summary(self):

        self.compute_scnt_data ()

        RSTtable (
            columns = stacnt_columns + (colid.ovsta,),
            contents = [self.scnt_dict ()]
            ).dump_to (self.rstf)

    # -------------------
    # -- gen_tcsummary --
    # -------------------

    def gen_tcsummary(self):
        self.rstf = RSTfile ("tcsummary.rst")
        self.gen_vcnt_summary()
        self.gen_scnt_summary()
        self.rstf.close()

    # -----------------
    # -- gen_envinfo --
    # -----------------

    def gen_envinfo(self):
        self.rstf = RSTfile ("envinfo.rst")
        self.rstf.close()

    # ---------------
    # -- gen_index --
    # ---------------

    def gen_index(self):
        self.rstf = RSTfile ("index.rst")

        self.rstf.write(
            rest.chapter('GNATcoverage Software Test Results'))

        self.rstf.close()



