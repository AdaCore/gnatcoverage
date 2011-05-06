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

import os, sys, pickle

from REST import rest

from SCOV.internals.cnotes import *

from SUITE.control import BUILDER

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


# =======================================
# == Qualification report and helpers  ==
# =======================================

# The core of our test-results report is a set of tables, matrix
# of rows over a fixed set of columns

# Column abstraction and instances, used both as column designators and
# descriptive data holders

class Column:
    def __init__(self, htext, legend=""):

        # HTEXT is the column header text, and LEGEND is a brief
        # description of the column contents

        self.htext = htext
        self.legend = legend

class colid:

    # Testcase description and status, for testcase table

    tc = Column (
        htext="testcase", legend="Testcase id")

    sta = Column (
        htext="status", legend="Test execution status")

    # Expectation counters, for testcase table and counters summary

    nov = Column (
        htext="nov", legend="# absence of violations")

    scv = Column (
        htext="scv", legend="# Statement Coverage violations")

    dcv = Column (
        htext="dcv", legend="# Decision Coverage violations")

    ccv = Column (
        htext="ccv", legend="# Condition Coverage violations")

    xbv = Column (
        htext="xbv", legend="# Exempted blocks with violations")

    # Status counters and overall status, for status summary

    qlevel = Column (
        htext="level", legend="qualification level")

    failed = Column (
        htext="failed", legend="# tests failed")

    passed = Column (
        htext="passed", legend="# tests passed")

    ovsta = Column (
        htext="overall", legend="Testsuite overall status")

    # Other status counters

    uok = Column (
        htext="uok", legend="Test ran OK despite expected to fail")

    xfail = Column (
        htext="xfail", legend="Test failed as expected")

    # Columns for legend sub-tables

    colname = Column (
        htext="column", legend=None)

    legend = Column (
        htext="legend", legend=None)

# ------------------------------------
# -- Mappings to column identifiers --
# ------------------------------------

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

    'OK'     : colid.passed,
    'XFAIL'  : colid.passed,
    'UOK'    : colid.passed,
    'FAILED' : colid.failed
}

# -------------------------------
# -- Useful sets of column ids --
# -------------------------------

# Violation counters
viocnt_columns = (colid.nov, colid.scv, colid.dcv, colid.ccv, colid.xbv)

# Status counters
stacnt_columns = (colid.passed, colid.failed)

# ----------------------------------
# -- Column contents abstractions --
# ----------------------------------

# These are used to offer a consistent interface despite variations of
# contents kind for different columns in a table row, e.g. an "img" method to
# return a string image of the contents with possible format variations.

# Cell to hold satisfied/total counters
# --------------------------------------

class CountersCell:

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

# Cell to hold a simple text
# --------------------------

class TextCell:
    def __init__(self, text):
        self.text = text

    def img(self, details=False):
        return "%s" % self.text

# Cell to hold a qualification status
# -----------------------------------

class QstatusCell:
    def __init__(self, text):

        # TEXT is the precise test execution status provided by the toplevel
        # driver (with uok, xfail, ...). Switch to the qualification status by
        # mapping to the status counter column.

        self.colid = column_for[text]

    def img(self, details=False):
        return "%s" % self.colid.htext

# =============
# == RSTfile ==
# =============

class RSTfile:
    def __init__(self, filename):
        self.fd = None
        self.open (filename)

    def open(self, filename):
        self.fd = open ("qreport/source/" + filename, 'w')

    def write(self, text, pre = 0, post = 1):
        self.fd.write (rest.isolate (
                text = text, pre = pre, post = post))

    def close(self):
        self.fd.close()

# ==============
# == RSTtable ==
# ==============

class RSTtable:

    def __init__(self, title, text, columns, contents):

        # COLUMNS is the list of columns for this table, CONTENTS is a list of
        # {col -> text} dictionaries, one per table line. Compared to keys()
        # in CONTENTS, COLUMNS is useful to enforce a provided order.

        self.columns = columns
        self.contents = contents

        # TITLE and TEXT are the table title and descriptive text, if any.
        # When TITLE is None, the table description is omitted as a whole,
        # legend included.

        self.title = title
        self.text  = text

    def __dump_description(self):

        # Dump the table title, descriptive text and legend

        self.rstf.write (rest.strong (self.title), post=2)
        self.rstf.write (self.text, post=2)

        # For text alignment purposes, the legend is best displayed
        # as a (description-less) table itself ...

        RSTtable (
            title = None, text = None,
            columns = (colid.colname, colid.legend),
            contents = [
                {colid.colname : rest.emph(col.htext),
                 colid.legend  : col.legend} for col in self.columns]
            ).dump_to (self.rstf)

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
        self.rstf.write (sepl, post=2)

    def __compute_widths(self):

        # Compute the current width of every column, as the max of
        # all the texts (on every contents + header line)

        # This is useful to align the text of every column, for both
        # content rows and the ReST table separation lines.

        self.width = dict (
            [(col, 0) for col in self.columns])

        # Maximize column width over contents entries

        [self.width.__setitem__ (
                col, max (self.width[col], len(centry[col])))
         for centry in self.contents for col in self.columns]

        # Maximize column width with length of column header

        [self.width.__setitem__ (
                col, max (self.width[col], len(col.htext)))
         for col in self.columns]

    # -------------
    # -- dump_to --
    # -------------

    def dump_to(self, rstf):

        self.rstf = rstf

        if self.text:
            self.__dump_description ()
            self.rstf.write("~", post=2)

        self.__compute_widths ()

        self.__dump_header ()
        self.__dump_contents ()
        self.__dump_footer ()

# ==============
# == QDreport ==
# ==============

class QDreport:

    def __init__(self, options, qdreg):
        self.options = options
        self.qdl = sorted (qdreg.qdl, key=lambda qd: qd.tcid)

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
    # Qualif/bla  3        1            passed
    # ...
    # =========== ======== ======== ... =======

    def count(self, note, cell):
        cell.total += 1
        if note.satisfied():
            cell.satisfied += 1

    def tcdata_for(self, qd):

        # Process one qualification data item, producing a report data line
        # for one testcase

        this_tcdata = dict (
            [(key, CountersCell()) for key in viocnt_columns])

        this_tcdata.__setitem__(colid.sta, QstatusCell(qd.status))

        [self.count (
                note = note, cell = this_tcdata [column_for[note.kind]])
         for qde in qd.entries for src in qde.xrnotes
         for notelist in qde.xrnotes[src].itervalues()
         for note in notelist]

        return this_tcdata

    def compute_tcdata(self):
        self.tcdata = dict (
            [(qd, self.tcdata_for (qd)) for qd in self.qdl])

    def tcdict_for(self, qd):
        r = dict()

        r.__setitem__(colid.tc, "%s" % qd.tcid)

        details = qd.status != 'OK'
        [r.__setitem__(col, "%s" % self.tcdata[qd][col].img(details))
         for col in self.tcdata[qd]]

        return r

    def gen_tctable(self):

        self.compute_tcdata()

        rstf = RSTfile ("tctable.rst")

        rstf.write (rest.chapter ("Testcase execution summary"))

        RSTtable (
            title = "Testcase Table",
            text = ' '.join (
                ["This table lists all the testcases that were executed.",
                 "It displays the execution status and a set of expectation",
                 "counters for each of them.",
                 "\n\n\"#\" in the legend denotes \"number of satisfied",
                 "expectations for ...\"."]),
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
        self.vcnts = dict (
            [(key, CountersCell()) for key in viocnt_columns])
        [self.do_vcnt(qd) for qd in self.qdl]

    def vcnt_dict(self):
        return dict (
            [(col, "%s" % self.vcnts[col].img()) for col in viocnt_columns])

    def gen_vcnt_summary(self):

        self.compute_vcnt_data ()

        RSTtable (
            title = "Expectation Counters Summary",
            text = ''.join (
                ["This table summarizes expectation counters across the ",
                 "entire set of executed tests. Its sums the number of ",
                 "satisfied expectations presented in the testcase table."]),
            columns = viocnt_columns,
            contents = [self.vcnt_dict()]
            ).dump_to (self.rstf)

    # ---------------------
    # -- gen_scnt_summary --
    # ---------------------

    # Compute and write out a status totals summary like
    #
    # ======== ======== =======
    # passed   failed   overall
    # ======== ======== =======
    # 3        1        OK
    # ======== ======== =======

    def do_scnt(self, qd):
        self.scnts[column_for[qd.status]] += 1

    def compute_scnt_data(self):
        self.scnts = dict (
            [(key, 0) for key in stacnt_columns])
        [self.do_scnt(qd) for qd in self.qdl]

    def scnt_dict(self):
        r = dict (
            [(col, "%d" % self.scnts[col]) for col in stacnt_columns])
        r.__setitem__ (
            colid.qlevel,
            "%s" % self.options.qualif_level)
        r.__setitem__ (
            colid.ovsta,
            "%s" % "OK" if self.scnts[colid.failed] == 0 else "BING")
        return r

    def gen_scnt_summary(self):

        self.compute_scnt_data ()

        RSTtable (
            title = "Overall Status",
            text = ''.join (
                ["This table sums the number of tests that passed or ",
                 "failed, as listed in the testcase table. It displays the"
                 "corresponding overall status of the entire testsuite."]),
            columns = (colid.qlevel,) + stacnt_columns + (colid.ovsta,),
            contents = [self.scnt_dict ()]
            ).dump_to (self.rstf)

    # -------------------
    # -- gen_tcsummary --
    # -------------------

    def gen_tcsummary(self):

        self.rstf = RSTfile ("tcsummary.rst")

        self.rstf.write (rest.chapter ("Testsuite status summary"))

        self.gen_scnt_summary()
        self.gen_vcnt_summary()
        self.rstf.close()

    # -----------------
    # -- gen_envinfo --
    # -----------------

    def gen_suite_options(self):

        item = Column (
            htext = "Suite control", legend = None)

        value = Column (
            htext = "", legend = None)

        RSTtable (
            title = None, text = None,
            columns = (item, value),
            contents = [
                {item : "qualification level",
                 value: self.options.qualif_level
                 },

                {item : "full command line",
                 value: ' '.join (sys.argv)
                 },

                {item : "compiler options",
                 value: ' '.join (
                        (BUILDER.COMMON_CARGS, BUILDER.SCOV_CARGS,
                         self.options.qualif_cargs
                         if self.options.qualif_cargs else ""))
                 }
                ]
            ).dump_to (self.rstf)

    def gen_suite_environ(self):

        # builder program name and version

        # compilation options

        # host environment

        item = Column (
            htext = "Environment item", legend = None)

        value = Column (
            htext = "Value", legend = None)

        RSTtable (
            title = None, text = None,
            columns = (item, value),
            contents = [{item : "--qualif-level",
                         value: ""}]
            ).dump_to (self.rstf)


    def gen_envinfo(self):
        self.rstf = RSTfile ("envinfo.rst")
        self.rstf.write (rest.chapter ("Execution environment"))

        self.gen_suite_options ()
        self.rstf.write ("~\n")
        self.gen_suite_environ ()

        self.rstf.close()

    # ---------------
    # -- gen_index --
    # ---------------

    def gen_index(self):
        self.rstf = RSTfile ("index.rst")

        self.rstf.write(rest.chapter('GNATcoverage Software Test Results'))

        self.rstf.write(rest.toctree(
                ["envinfo.rst", "tctable.rst", "tcsummary.rst"], depth = 1))

        self.rstf.close()



