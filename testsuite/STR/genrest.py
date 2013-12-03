# *****************************************************************************

# This is a helper script aimed at the production of the STR (Software Test
# Result) document for GNATcoverage, part of its qualification material.

# It fetches test results and context information from a directory where an
# actual testsuite execution was run and generates a REST document from there.

# *****************************************************************************

import sys, os, re, optparse, collections
from gnatpython.env import Env

# The testsuite dir corresponding to this local "qualification" dir. This is
# where we fetch common python source dirs. This might be different from where
# the actual testsuite results could be found, as provided through
# --testsuite-dir, though the two should be consistent wrt the python script
# sources (e.g. the CTXdata structure must match)

LOCAL_TESTSUITE_DIR=os.path.abspath("../../testsuite")
sys.path.append(LOCAL_TESTSUITE_DIR)

from SUITE.qdata import qdaf_in, stdf_in
from SUITE.qdata import QUALDATA_FILE, QLANGUAGES, QROOTDIR
from SUITE.qdata import CTXDATA_FILE
from SUITE.cutils import to_list
from SUITE.control import LANGINFO, XCOV

from SUITE import dutils

from SCOV.internals.cnotes import *
from REST import rest

# =============================
# == Misc, utility functions ==
# =============================

class Error (Exception):
    def __init__(self):
        pass

def fail_if (p, msg):
    if p:
        print "!!! %s !!!" % msg
        raise Error

def exit_if (p, msg):
    if p:
        print msg
        sys.exit(1)

# ================================================
# == QDregistry - Qualification Data repository ==
# ================================================

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

class QDregistry_from:

    def __init__(self, testsuite_dir):

        # The full qualification data for this report; sequence of testcase
        # Qdata instances:
        self.qdl = []

        self.__fetch_qdinstances_from(root=testsuite_dir)

    def __fetch_qdinstances_from(self, root):

        print "== Registering test execution dumps from %s ..." % root
 
        [self.__fetch_one_qdinstance_from(dirname)
         for (dirname, subdirs, files) in os.walk(
                top=root, topdown=True, followlinks=True)
         if QUALDATA_FILE in files]


    def __fetch_one_qdinstance_from(self, dirname):

        print "loading from %s" % dirname

        qda = dutils.pload_from (qdaf_in (dirname))
        std = dutils.pload_from (stdf_in (dirname))

        qda.status = std.status
        qda.comment = std.comment
        
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
        htext="testcase", legend="Testcase Identifier")

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

# Violation counters we care about for each possible do-level
viocnt_columns_for = {
    'doA': (colid.nov, colid.igv, colid.scv, colid.dcv, colid.mcv, colid.xbv),
    'doB': (colid.nov, colid.igv, colid.scv, colid.dcv, colid.xbv),
    'doC': (colid.nov, colid.igv, colid.scv, colid.xbv)
}

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

# Cell to hold a TC id in a context where the language
# and test category are explicit by some other means
# -----------------------------------------------------

class TcidCell:
    def __init__(self, text):
        self.text = text

    def img(self, details=False):

        # Arrange to strip:
        #
        # * The common leading part that can be inferred from the tc
        #   language and category. Qualif/Ada/stmt, for example, is just
        #   redundant within the STATEMENT section for Ada.
        #
        # * The <digits>_ sequences at the beginning of subdir names,
        #   introduced for sorting purposes and not displayed in the TOR
        #   hierarchy.

        # For example:
        # Qualif/Ada/stmt/1_Core/11_Exceptions -> Core/Exceptions

        # Ordered dict of { re_toreplace -> what_to_replace_with }
        # Applied in sequence and the order matters.
        
        re_strip = collections.OrderedDict (
            ((QROOTDIR + "/", ""),
             ("(Ada|C)/(stmt|decision|mcdc)/", ""),
             ("Common/Report/", ""),
             ("^[0-9]+_", ""),
             ("/[0-9]+_", "/"))
            )
        img=self.text
        for r in re_strip:
            img = re.sub(pattern=r, repl=re_strip[r], string=img)

        return img

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
        self.fd = open ("source/" + filename, 'w')

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

        # Compute the QM testcase identifier prefix for this category.
        # ??? yes, this kind of manual synchronization between what the
        # QM produces in the TOR document vs what we display in the STR
        # doc here for testcase identification is a pain.
        
        self.qmprefix = self.matcher.replace (QROOTDIR, "/TOR")

        self.qdl = []

    def trymatch(self, qda):
        if re.search(self.matcher, qda.tcid):
            self.qdl.append (qda)
            return True
        else:
            return False

class QDreport:

    def __init__(self, options):

        self.o = options
        
        # Fetch the testsuite execution context

        self.suitedata = dutils.jload_from (
            os.path.join (self.o.testsuite_dir, CTXDATA_FILE))

        # Pick the testsuite dolevel if none was provided. Check
        # consistency otherwise:

        suite_dolevel = self.suitedata['options']['dolevel']
        
        fail_if (
            not suite_dolevel,            
            "Testsuite at %s was not run in qualification mode" % (
                self.o.testsuite_dir)
            )

        if self.o.dolevel == None:
            self.o.dolevel = suite_dolevel
        else:
            fail_if (
                self.o.dolevel != suite_dolevel,
                "explicit dolevel (%s) doesn't match testsuite (%s)" % (
                    self.o.dolevel, suite_dolevel)
                )

        # Setup the list of violation counters we care about wrt the
        # targetted do-level:
            
        self.viocnt_columns = viocnt_columns_for[self.o.dolevel]
            
        # Fetch the test results:

        qdreg = QDregistry_from(testsuite_dir=self.o.testsuite_dir)
        self.qdl = sorted (qdreg.qdl, key=lambda qd: qd.tcid)

        self.rstf = None

        # All the parts of our report get structured with sections
        # based on test categories. We distinguish language specific
        # categories from common ones.

        lang_categories = [
            (Category (
                    lang=lang, name="%s STATEMENT Coverage" % lang,
                    matcher="%s/%s/stmt" % (QROOTDIR, lang)),
             Category (
                    lang=lang, name="%s DECISION Coverage" % lang,
                    matcher="%s/%s/decision" % (QROOTDIR, lang)),
             Category (
                    lang=lang, name="%s MCDC Coverage" % lang,
                    matcher="%s/%s/mcdc" % (QROOTDIR, lang))
             ) for lang in QLANGUAGES
            ]

        lang_categories = [
            cat for lang_cats in lang_categories for cat in lang_cats]

        other_categories = [
            Category (
                name="REPORT Format", 
                matcher="%s/Common/Report" % QROOTDIR),

            Category (
                name="Testsuite Selftest",
                matcher="%s/Appendix/Testsuite" % QROOTDIR,
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
    # { qd -> { colid.tc: TcidCell(qd.tcid),
    #           colid.nov: CountersCell(),
    #           ...
    #           colid.sta: QstatusCell(qd.status)
    #         }
    #   ...
    # }

    def tccolumns(self):
        return (colid.tc,) + self.viocnt_columns + (colid.sta,)

    def count(self, note, cell):
        cell.expected += 1
        if note.satisfied():
            cell.satisfied += 1

    def tcdata_for(self, qd):

        # Process one qualification data item, producing a report data line
        # for one testcase

        this_tcdata = dict (
            [(colid.tc, TcidCell (qd.tcid))]
            + [(key, CountersCell()) for key in self.viocnt_columns]
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

        # Arrange to provide details on the expected versus satisfied
        # violation counts only when the test has failed somehow:

        details = column_for[qd.status] != colid.passed

        return dict(
            [(col, "%s" % self.tcdata[qd][col].img(details))
             for col in self.tcdata[qd]])

    def gen_tctables(self, sepfile=None):

        if sepfile:
            self.rstf = RSTfile (sepfile)

        self.rstf.write (".. _tctable:\n")

        self.rstf.write (rest.chapter ("Testcase Execution summary"))

        # Arrange to get a single description and legend followed by a set of
        # tables with data for each category.

        RSTtable (
            title = None,
            text = (
                "The following tables list all the testcases that were "
                "executed, with their execution status and a set of "
                "expectation counters. '# <...>' in the legend abbreviates "
                "\"number of satisfied expectations for <...>\". "
                "\n\n"
                "\"Expectations\" here refers to expected coverage results "
                "stated as embedded comments within the test sources. "
                "\"Violations\" in this context refer to "
                "deviations with respect to a given coverage criterion, "
                "that tests trigger on purpose and which we expect the "
                "tool to detect. For example, a test arranging not to execute "
                "a specific statement triggers a statement coverage violation "
                "on purpose, which the tool must report for the test to pass. "
                "The `scv` counter for a testcase, for "
                "instance, is then the number of statement coverage "
                "violations triggered on purpose by the tests executed for "
                "this testcase, that our testsuite harness has verified "
                "the tool did detect."
                "\n\n"
                "The set of tables is partitioned according to "
                "language/coverage-criterion associations described by "
                "section titles. The text in square brackets at the end "
                "of each section title is a common prefix to the Testcase "
                "Identifier column, just not repeated on every line. These "
                "identifiers match those used in the TOR document so can "
                "be used to look up results from testcase descriptions or "
                "vice-versa."
                "\n\n"
                "A general summary of all the results is provided in the "
                ":ref:`tssummary` section of this report.\n"
                ),
            columns = self.tccolumns(),
            contents = None,
            ).dump_to (self.rstf)

        [RSTtable (
                title = ("%s tests [ %s/... ]" % (cat.name, cat.qmprefix)),
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
        return (colid.cat,) + stacnt_columns + (colid.ovsta,) + self.viocnt_columns

    def init_data_for(self, catid):

        # Initial data for the line corresponding to category CATID. The
        # overall status column is set separately, once the final counter
        # values are known

        return dict (
            [(colid.cat, TextCell(catid))]
            + [(col, IntCell()) for col in stacnt_columns]
            + [(col, CountersCell()) for col in self.viocnt_columns]
            )

    def do_sum(self, qd, catsum):
        [catsum[key].augment_by(self.tcdata[qd][key])
         for key in self.viocnt_columns]
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
                "%s" % "OK" if thissum[colid.failed].value == 0 else "NOT OK"))

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
                "%s" % "OK" if totsum[colid.failed].value == 0 else "NOT OK"))

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

        self.rstf.write (".. _tssummary:\n")

        self.rstf.write (rest.chapter ("Testsuite Status summary"))

        RSTtable (
            title = None,
            text = \
                "This table summarizes status and expectation counters "
                "for each test category across the entire testsuite.  Please "
                "refer to the :ref:`tctable` section of ths report for an "
                "explanation of the legend. ",
            columns = self.sumcolumns(),
            contents = self.sumcontents()
            ).dump_to (self.rstf)

        if sepfile:
            self.rstf.close()

    # -----------------
    # -- gen_envinfo --
    # -----------------

    def gen_suite_options(self):

        def literal(text):
            return ":literal:`" + text.strip() + "`"

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

        suite_options = self.suitedata['options']

        csv_contents = []

        csv_contents.append(
            {item : "testsuite execution command line",
             value: literal(self.suitedata['cmdline'])
             })

        csv_contents.append(
            {item : "compiler switches - language-independent",
             value: literal (' '.join (
                    BUILDER.COMMON_CARGS() + [suite_options['cargs']]))
             })

        for lang in self.languages:
            lang_cargs = suite_options["cargs_%s" % lang]
            
            if lang_cargs:
                csv_contents.append(
                    { item : "compiler switches - %s specific" % lang,
                      value: literal(lang_cargs)
                      })
        
        CSVtable (
            title = None, text = None,
            columns = (item, value),
            controls = [":widths: 30, 65"],
            contents = csv_contents
            ).dump_to (self.rstf)

    def gen_suite_environ(self):

        # We construct a table like
        #
        # Environment item | <data 1, no title> | <data 2, no title>
        # -----------------------------------------------------------
        # gnatpro exe name | gnatpro exe name   | gnatpro version
        # & version        |                    |
        # ...

        item = Column (
            htext = "Environment items", legend = None)

        v1 = Column (
            htext = "", legend = None)

        v2 = Column (
            htext = "", legend = None)

        suite_gnatpro = self.suitedata['gnatpro']
        suite_gnatcov = self.suitedata['gnatcov']
        suite_gnatemu = self.suitedata['gnatemu']
        suite_other   = self.suitedata['other']
        
        # Base table entries, always there:

        table_entries =  [
            {item : "testsuite execution timestamp & host system",
             v1: self.suitedata['runstamp'],
             v2: self.suitedata['host']
             },
            {item : "GNAT Pro executable & version",
             v1: suite_gnatpro['exename'],
             v2: suite_gnatpro['version']
             },
            {item : "GNATcov executable & version",
             v1: suite_gnatcov['exename'],
             v2: suite_gnatcov['version']
             }
            ]

        # Add a gnatemu version, unless known to be irrelevant (native,
        # or when a --board option is passed).

        suite_options = self.suitedata['options']
        if suite_options['target'] and not suite_options['board']:
            table_entries.append (
                {item : "GNATemu executable & version",
                 v1: suite_gnatemu['exename'],
                 v2: suite_gnatemu['version']}
                )

        # Add the "other tool" version if we have it

        if suite_other:
            table_entries.append (
                {item : "Other toolset executable & version",
                 v1: suite_other['exename'],
                 v2: suite_other['version']}
                )

        CSVtable (
            title = None, text = None,
            columns = (item, v1, v2),
            controls = [":widths: 25, 20, 55"],
            delim = '|',
            contents = table_entries
            ).dump_to (self.rstf)

        # ??? qemu-system-ppc ???

    def gen_envinfo(self, sepfile=None):

        if sepfile:
            self.rstf = RSTfile (sepfile)

        self.rstf.write (rest.chapter ("Qualification Environment"))

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

# ======================
# == main entry point ==
# ======================

valid_dolevels   = ('doA', 'doB', 'doC')

if __name__ == "__main__":

    op = optparse.OptionParser(usage="%prog <options>")

    op.add_option (
        "--dolevel", dest="dolevel", default=None,
        type='choice', choices=valid_dolevels,
        help = (
            "Target DO178 qualification level. "
            "Defaults to the one of testsuite-dir.")
        )

    default_testsuite_dir = LOCAL_TESTSUITE_DIR
    op.add_option (
        "--testsuite-dir", dest="testsuite_dir", default=default_testsuite_dir,
        help = (
            "Name of a directory where the testsuite was run. "
            "Defaults to \"%s\"." % default_testsuite_dir)
        )

    (options, args) = op.parse_args()

    QDreport(options=options)


