# ****************************************************************************
# **                TESTSUITE QUALIFICATION-DATA FACILITIES                 **
# ****************************************************************************

# This module exposes the testsuite qualification data management facilities,
# aimed at the production of a test-results report at the end of the testsuite
# execution.
#
# Testcases dump Qdata instances when they execute. The testsuite driver
# retrieves and accumulates them on the fly, then produces a report from the
# complete set at the end.

# ****************************************************************************

import os, sys, pickle, re

from REST import rest

from SUITE.cutils import to_list
from SUITE.control import LANGINFO, XCOV

from SCOV.internals.cnotes import *

QLANGUAGES = [li.name for li in LANGINFO.values() if li.scos_ext]
# list of languages we support qualification tests for

QROOTDIR="Qualif"
# String that identifies a qualification test at the beginning of it's
# sub-directory name relative to the testsuite root

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

    def __init__(self, xfile, drivers, xrnotes, wdir):
        self.xfile = xfile       # expectation file
        self.drivers = drivers   # drivers run to satisfy them
        self.xrnotes = xrnotes   # expected report notes, KnoteDict per source
        self.wdir    = wdir      # working directory where the tc ran

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
    def __init__(self, htext, legend="", hidden=False):

        # HTEXT is the column header text,
        # LEGEND is a brief description of the column contents,
        # HIDDEN tells if the column should not be displayed

        self.htext = htext
        self.legend = legend
        self.hidden = hidden

class colid:

    # Testcase description and status, for testcase table

    tc = Column (
        htext="testcase", legend="Testcase id")

    sta = Column (
        htext="status", legend="Test execution status")

    cat = Column (
        htext="category", legend="Test category")

    # Expectation counters, for testcase table and counters summary

    nov = Column (
        htext="nov", legend="# absence of violations")

    scv = Column (
        htext="scv", legend="# STATEMENT Coverage violations")

    dcv = Column (
        htext="dcv", legend="# DECISION Coverage violations")

    mcv = Column (
        htext="mcv", legend="# MCDC Coverage violations")

    xbv = Column (
        htext="xbv", legend="# EXEMPTION blocks with violations")

    igv = Column (
        htext="igv", legend="counters to be ignored", hidden=True)

    # Status counters and overall status, for status summary

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
    r0c     : colid.igv,
    xBlock0 : colid.nov,

    sNoCov   : colid.scv,
    sPartCov : colid.scv,

    dtNoCov  : colid.dcv,
    dfNoCov  : colid.dcv,
    dPartCov : colid.dcv,
    dNoCov   : colid.dcv,

    etNoCov  : colid.mcv,
    efNoCov  : colid.mcv,
    ePartCov : colid.mcv,
    eNoCov   : colid.mcv,
    cPartCov : colid.mcv,

    xBlock1  : colid.xbv,

    # When checking status, map text passed by toplevel driver
    # to column.

    'OK'     : colid.passed,
    'XFAIL'  : colid.passed,
    'UOK'    : colid.passed,
    'FAILED' : colid.failed
}

# -------------------------------
# -- Useful sets of column ids --
# -------------------------------

# Violation counters
viocnt_columns = (
    colid.nov, colid.igv, colid.scv, colid.dcv, colid.mcv, colid.xbv
    )

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
        self.expected = 0
        self.satisfied = 0

    def img(self, details=False):
        if details:
            return "%d/%d" % (self.satisfied, self.expected)
        else:
            return "%d" % self.expected

    def augment_by(self, other):
        self.expected += other.expected
        self.satisfied += other.satisfied

# Cell to hold a simple integer value
# -----------------------------------

class IntCell:

    def __init__(self, initval=0):
        self.value = initval

    def img(self, details=False):
        return "%d" % self.value

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

        # COLUMNS is the list of columns for this table, CONTENTS is a
        # list of {col -> text} dictionaries, one per table line.  Compared
        # to CONTENTS.keys(), COLUMNS is useful to enforce a column order.

        # CONTENTS may be None, which is useful to display a common
        # title/text/legend for a set of tables with contents coming right
        # after.

        self.columns = [col for col in columns if not col.hidden]
        self.contents = contents

        # TITLE and TEXT are the table title and descriptive text, if any.
        # Both may be None. The table legend is omitted when TEXT is None.

        self.title = title
        self.text  = text

    def __dump_description(self):

        # Dump the table title, descriptive text and legend

        if self.title:
            self.rstf.write (rest.section (self.title), post=2)

        if self.text is None:
            return

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

    def __dump_contents(self):
        [self.dump_centry (ce) for ce in self.contents]

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

    # -------------------------------------------
    # -- Internals to be specialized as needed --
    # -------------------------------------------

    # Version to generate "simple" table here

    def dump_header(self):
        sepl = " ".join (
            ["=" * self.width[col] for col in self.columns])

        headl = " ".join (
            ["%-*s" % (self.width [col], col.htext)
             for col in self.columns])

        self.rstf.write (sepl)
        self.rstf.write (headl)
        self.rstf.write (sepl)

    def dump_centry(self, ce):
        entryl = " ".join (
            ["%-*s" % (self.width[col], ce[col]) for col in self.columns])
        self.rstf.write (entryl)

    def dump_footer(self):
        sepl = " ".join (
            ["=" * self.width[col] for col in self.columns])
        self.rstf.write (sepl, post=2)

    # -------------
    # -- dump_to --
    # -------------

    def dump_to(self, rstf):

        self.rstf = rstf

        self.__dump_description ()

        # If we have a legend and contents, separate the two:
        if self.text is not None and self.contents is not None:
            self.rstf.write("~", post=2)

        if self.contents is not None:
            self.__compute_widths ()
            self.dump_header ()
            self.__dump_contents ()
            self.dump_footer ()

# ==============
# == CSVtable ==
# ==============

class CSVtable (RSTtable):

    def __init__(self, title, text, columns, contents,
                 delim = '&', controls = ()):

        self.controls = controls
        self.delim = delim
        RSTtable.__init__ (
            self, title=title, text=text,
            columns=columns, contents=contents)

    def dump_header(self):
        text = '\n' + '\n'.join (
            ['.. csv-table::',
             '   :delim: ' + self.delim,
             '   :header: %s' % " , ".join (
                    ['"%s"' % col.htext for col in self.columns])
             ] + ['   ' + ctl for ctl in self.controls]
            ) + "\n"
        self.rstf.write (text)

    def dump_centry(self, ce):
        entryl = (" %s " % self.delim).join (
            ["   %s" % ce[col] for col in self.columns])
        self.rstf.write (entryl)

    def dump_footer(self):
        self.rstf.write ('\n')


# ==============
# == QDreport ==
# ==============

import time, datetime, platform, socket

from gnatpython.env import Env
from gnatpython.ex import Run
from SUITE.control import BUILDER, LANGINFO
from SUITE.cutils  import version

# "Category" to which each testcase belongs, and which drives the segmentation
# of the qualification report

class Category:
    def __init__(self, name, matcher, lang=None, internal=False):
        self.lang     = lang
        self.name     = name
        self.matcher  = matcher
        self.internal = internal

        self.qdl = []

    def trymatch(self, qda):
        if re.search(self.matcher, qda.tcid):
            self.qdl.append (qda)
            return True
        else:
            return False

class QDreport:

    def __init__(self, options, qdreg):
        self.options = options
        self.qdl = sorted (qdreg.qdl, key=lambda qd: qd.tcid)

        self.rstf = None

        # All the parts of our report get structured with sections
        # based on test categories. We distinguish language specific
        # categories from common ones.

        lang_categories = [
            (Category (
                    lang=lang, name="%s - STATEMENT Coverage" % lang,
                    matcher="%s/%s/stmt" % (QROOTDIR, lang)),
             Category (
                    lang=lang, name="%s - DECISION Coverage" % lang,
                    matcher="%s/%s/decision" % (QROOTDIR, lang)),
             Category (
                    lang=lang, name="%s - MCDC Coverage" % lang,
                    matcher="%s/%s/mcdc" % (QROOTDIR, lang))
             ) for lang in QLANGUAGES
            ]

        lang_categories = [
            cat for lang_cats in lang_categories for cat in lang_cats]

        other_categories = [
            Category (
                name="REPORT Format", matcher="Qualif/Common/Report"),

            Category (
                name="Harness Check", matcher="Qualif/Appendix/Harness",
                internal=True),

            Category (
                name="Others", matcher=".")
            ]

        self.categories = lang_categories + other_categories

        [self.categorize(qda) for qda in self.qdl]

        # Compute the set of languages for which tests have matched (that is,
        # were run). This is useful e.g. to decide which sets of compilation
        # options should be displayed in the environment description items.

        self.languages = set (
            [cat.lang for cat in lang_categories if cat.qdl])

        self.gen_envinfo(sepfile="env.rst")

        # Then compute the tables and summaries of test status counters

        self.compute_tcdata()

        self.gen_tctables(sepfile="tctables.rst")
        self.gen_tssummary(sepfile="tssummary.rst")

        # Rely on a static toplevel index instead of generating one here.
        # This is much simpler to tailor and the harcoded knowledge (set of
        # sections/documents) is very limited and unlikely to change.

        # self.gen_index(sepfiles=["env.rst", "tctables.rst", "tssummary.rst"])

    def categorize(self, qda):
        for cat in self.categories:
            if cat.trymatch(qda):
                return

        raise FatalError (
            comment="unable to categorize testcase %s" % qda.tcid)

    # --------------------
    # -- compute_tcdata --
    # --------------------

    # The base data we will be working over: a dictionary associating each
    # testcase qualification data with a dictionary of it's execution summary
    # cells (one for each column). Something like:
    #
    # { qd -> { colid.tc: TextCell(qd.tcid),
    #           colid.nov: CountersCell(),
    #           ...
    #           colid.sta: QstatusCell(qd.status)
    #         }
    #   ...
    # }

    def tccolumns(self):
        return (colid.tc,) + viocnt_columns + (colid.sta,)

    def count(self, note, cell):
        cell.expected += 1
        if note.satisfied():
            cell.satisfied += 1

    def tcdata_for(self, qd):

        # Process one qualification data item, producing a report data line
        # for one testcase

        this_tcdata = dict (
            [(colid.tc, TextCell (qd.tcid))]
            + [(key, CountersCell()) for key in viocnt_columns]
            + [(colid.sta, QstatusCell(qd.status))]
            )

        [self.count (
                note = note, cell = this_tcdata [column_for[note.kind]])
         for qde in qd.entries for src in qde.xrnotes
         for notelist in qde.xrnotes[src].itervalues()
         for note in notelist]

        return this_tcdata

    def compute_tcdata(self):
        self.tcdata = dict (
            [(qd, self.tcdata_for (qd)) for qd in self.qdl])

    # ------------------
    # -- gen_tctables --
    # ------------------

    # For each category, compute and write out a testcase table like
    #
    # =========== ======== ======== ... =======
    # testcase    nov      scv          status
    # =========== ======== ======== ... =======
    # Qualif/bla  3        1            passed
    # ...
    # =========== ======== ======== ... =======

    def tcdict_for(self, qd):
        details = column_for[qd.status] != colid.passed
        return dict(
            [(col, "%s" % self.tcdata[qd][col].img(details))
             for col in self.tcdata[qd]])

    def gen_tctables(self, sepfile=None):

        if sepfile:
            self.rstf = RSTfile (sepfile)

        self.rstf.write (rest.chapter ("Testcase Execution summary"))

        # Arrange to get a single description and legend followed by a set of
        # tables with data for each category.

        RSTtable (
            title = None,
            text = \
                "The following tables list all the testcases that were "
                "executed, with their execution status and a set of "
                "expectation counters. '#' in the legend abbreviates "
                "\"number of satisfied expectations for ...\".",
            columns = self.tccolumns(),
            contents = None,
            ).dump_to (self.rstf)

        [RSTtable (
                title = "%s tests" % cat.name,
                text = None,
                columns = self.tccolumns(),
                contents = [self.tcdict_for(qd) for qd in cat.qdl]
                ).dump_to (self.rstf)
         for cat in self.categories if cat.qdl and not cat.internal]

        if sepfile:
            self.rstf.close()

    # -------------------
    # -- gen_tssummary --
    # -------------------

    # Compute and write out an overall summary like
    #
    # =========  ======= ======= ======= === === ...
    # category   #passed #failed overall nov scv ...
    # =========  ======= ======= ======= === === ...
    # Statement  3       1       NOK
    # ...
    # Total      ...
    # =========  ======= ======= ======= === === ...

    def sumcolumns(self):
        return (colid.cat,) + stacnt_columns + (colid.ovsta,) + viocnt_columns

    def init_data_for(self, catid):

        # Initial data for the line corresponding to category CATID. The
        # overall status column is set separately, once the final counter
        # values are known

        return dict (
            [(colid.cat, TextCell(catid))]
            + [(col, IntCell()) for col in stacnt_columns]
            + [(col, CountersCell()) for col in viocnt_columns]
            )

    def do_sum(self, qd, catsum):
        [catsum[key].augment_by(self.tcdata[qd][key])
         for key in viocnt_columns]
        catsum[column_for[qd.status]].value += 1

    def sumdata_for(self, cat, totsum):

        # Compute the summary data for category CAT, and accumulate
        # into the TOTSUM total summary along the way

        thissum = self.init_data_for(cat.name)

        [self.do_sum (qd=qd, catsum=thissum) for qd in cat.qdl]

        # skip internal categories unless they exhibit failures

        if cat.internal and thissum[colid.failed].value == 0:
            return None

        # Otherwise, update the overall status column, accumulate into the
        # TOTSUM summary and return

        thissum.__setitem__ (
            colid.ovsta, TextCell (
                "%s" % "OK" if thissum[colid.failed].value == 0 else "BING"))

        [self.do_sum (qd=qd, catsum=totsum) for qd in cat.qdl]

        return thissum

    def sumdata(self):

        # Compute the base data for the summary table. This is a list of
        # {colid -> Cell} dictionaries, with one list entry for each test
        # category + a total. Something like

        # [ { colid.cat: TextCell("stmt"), colid.passed: IntCell() ... },
        #   { colid.cat: TextCell("decision"), colid.passed: IntCell() ... }
        #   { colid.cat: TextCell("total"),  colid.passed: IntCell() ... }
        # ]

        # Allocate the total summary now, and update it together with each
        # category as we go

        totsum = self.init_data_for("Total")

        catsums = [
            csum for csum in [self.sumdata_for(cat, totsum)
                              for cat in self.categories if cat.qdl]
            if csum]

        totsum.__setitem__ (
            colid.ovsta, TextCell (
                "%s" % "OK" if totsum[colid.failed].value == 0 else "BING"))

        return catsums + [totsum]

    def sumcontents(self):

        # Compute the list of { colid -> text } dictionaries for the summary
        # table, with a list entry for each test category (+ total)

        return [dict ([(col, "%s" % catsum[col].img())
                       for col in self.sumcolumns()])
                for catsum in self.sumdata()]

    def gen_tssummary(self, sepfile):

        if sepfile:
            self.rstf = RSTfile (sepfile)

        self.rstf.write (rest.chapter ("Testsuite Status summary"))

        RSTtable (
            title = None,
            text = \
                "This table summarizes status and expectation counters "
                "for each test category across the entire testsuite.",
            columns = self.sumcolumns(),
            contents = self.sumcontents()
            ).dump_to (self.rstf)

        if sepfile:
            self.rstf.close()

    # -----------------
    # -- gen_envinfo --
    # -----------------

    def gen_suite_options(self):

        item = Column (
            htext = "Suite control items", legend = None)

        value = Column (
            htext = "", legend = None)

        # Some options are really language specific (e.g. -gnatp, -gnateS,
        # ...) and we need to split these out, as dumping a single catenation
        # of all the possibilities makes no sense.  Besides, we might be
        # producing qualification results only for a subset all the possible
        # supported languages, and we want to omit the specifics corresponding
        # to languages out of that subset.

        # Options might be coming from a variety of places:
        # - BUILDER.COMMON_CARGS (e.g. -fpreserve-control-flow),
        # - LANGINFO.cargs (e.g. -gnateS for Ada, -fdump-scos for C)
        # - --cargs family

        options = self.options.__dict__

        CSVtable (
            title = None, text = None,
            columns = (item, value),
            controls = [":widths: 30, 65"],
            contents = [
                {item : "testsuite execution command line",
                 value: ' '.join (sys.argv)
                 },
                {item : "compiler switches - language agnostic",
                 value: ' '.join (
                        (BUILDER.COMMON_CARGS,
                         options["cargs"])
                        )
                 } ] + \
                [ { item : "compiler switches - %s specific" % lang,
                    value: options["cargs_%s" % lang]
                  } for lang in self.languages ]
            ).dump_to (self.rstf)

    def gen_suite_environ(self):

        item = Column (
            htext = "Environment items", legend = None)

        v1 = Column (
            htext = "", legend = None)

        v2 = Column (
            htext = "", legend = None)

        gnatpro = Env().target.triplet + "-gcc"
        gnatemu = Env().target.triplet + "-gnatemu"
        gnatcov = XCOV

        CSVtable (
            title = None, text = None,
            columns = (item, v1, v2),
            controls = [":widths: 25, 20, 55"],
            delim = '|',
            contents = [
                {item : "report timestamp & host system",
                 v1: time.strftime ("%a %b %d, %Y. %H:%M", time.localtime()),
                 v2: ' '.join (
                        (platform.system(), platform.release()))
                 },
                {item : "GNAT Pro executable & version",
                 v1: gnatpro,
                 v2: version(gnatpro)
                 },
                {item : "GNATcov executable & version",
                 v1: gnatcov,
                 v2: version (gnatcov)
                 },
                {item : "GNATemu executable & version",
                 v1: gnatemu,
                 v2: version(gnatemu)
                 }
                ]
            ).dump_to (self.rstf)

        # ??? qemu-system-ppc ???

    def gen_envinfo(self, sepfile=None):

        if sepfile:
            self.rstf = RSTfile (sepfile)

        self.rstf.write (rest.chapter ("Execution Context summary"))

        self.gen_suite_options ()
        self.rstf.write ("~\n")
        self.gen_suite_environ ()

        if sepfile:
            self.rstf.close()

    # ---------------
    # -- gen_index --
    # ---------------

    def gen_index(self, sepfiles):

        self.rstf = RSTfile ("index.rst")

        self.rstf.write(rest.chapter('GNATcoverage Software Test Results'))

        self.rstf.write(rest.toctree(sepfiles, depth = 1))

        self.rstf.close()

