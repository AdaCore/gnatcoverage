"""
EMITTED LNOTE EXPANDERS

Expose the XnotesExpander class which constructs two { source -> KnoteDict }
dictionaries of expected coverage notes, one for line notes and one for
report notes as expressed by user expectations found in a provided XFILE.
"""

import os
import re
import shlex

from SUITE.context import thistest
from SUITE.control import language_info
from SUITE.cutils import FatalError, lines_of
from .cnotes import (
    KnoteDict,
    dNoCov,
    dPartCov,
    dfNoCov,
    dtNoCov,
    eNoCov,
    ePartCov,
    efNoCov,
    etNoCov,
    lPartCov,
    oNoCov,
    oPartCov,
    ofNoCov,
    otNoCov,
    sNoCov,
    xlNoteKinds,
    xrNoteKinds,
)
from .tfiles import Tfile
from .xnotep import XnoteP

# We refer to the expressed user expectations as SCOV data, and parse it
# according to the following grammar:

#     SCOV.data := ucxgroup__list
#     ucxgroup__list := ucx_group <newline> [ucxgroup__list]
#     ucx_group := sources <new_line> lx_or_ctl__list

#     sources := "--# " filename_lists
#     filename_lists := filename_list ["|" filename_lists]
#     filename_list := FILENAME [ filename_list]

#     lx_or_ctl__list := lx_or_ctl <newline> [lx_or_ctl__list]
#     lx_or_ctl := lx | cont | ctl

#     ctl := "-- " arg_ctl__list
#     arg_ctl__list := arg_ctl ["## " arg_ctl__list]
#     arg_ctl := "%"<cargs|cov|tags|cancel>":" optgroup__list
#     optgroup__list := optgroup ["," optgroup__list]
#     optgroup := ["!"]<option sequence like "-gnatp" or "-S routines">

#     cont := "--" <whitespaces> "+#" lx_rnote_list <newline>

#     lx := "-- " lx_lre lx_lnote_list " ## " lx_rnote_list <newline>
#     lx_lre := ["="]"/" REGEXP "/"
#     weak_mark := ~

#     cov_level_choice := <s|d|m|u>
#     cov_level_list := cov_level_choice [cov_level_list]
#     cov_level_test := cov_level_list "=>"

#     lx_lnote_list := lx_lnote_choice ["," lx_lnote_list]
#     lx_lnote_choice := [cov_level_test] [weak_mark] lx_lnote
#     lx_lnote := <l-|l!|l+|l*|l#|l@|l0|l?|l=>

#     lx_rnote_list := lx_rnote_choice [rnote_sep lx_rnote_list]
#     lx_rnote_choice := [cov_level_test] [weak_mark] lx_rnote
#     lx_rnote_kind := <s-|s!|s?|dT-|dF-|d!|d?|eT-|eF-|e?|oT-|oF-|c!|x0|x+|x?>
#     lx_ernote_kind := <Xs-|Xs!|Xs0|Xs?|XoT-|XoF-|Xo!|Xo-|Xc!|X0|X0c>
#     lx_rnote := <lx_rnote_kind|lxernote_kind>[:"TEXT"][@(STAG)]

#     rnote_sep is selected to be '#' if any # is found on the line,
#                              or ',' if any , is found on the line,
#     crude but good enough so far.

# The start of the SCOV data is identified as the first comment whose syntax
# matches a "sources" line.  Any comment before then is assumed to be a normal
# free-text comment.  Any comment thereafter is assumed to be part of the
# SCOV data.

# SOURCES
# -------
# Expectations apply to sets of sources conveyed by the "sources" item of our
# grammar. Source files are searched from standards paths for a testcase (src/
# ../src/ etc) and so are most often referenced by their basename in
# expectations.
#
# A group of expectations may apply to several sources, just listed in sequence
# as in:
#
#   -- # x0.adb y0.adb        : in x0.adb and y0.adb, expect ...
#
# Alternatives sets, useful for shared drivers, are allowed for a given group.
# Sets are separated with '|' as in:
#
#   -- # x0.adb | x1.c y1.c   : in (x0.adb) or (x1.c and y1.c), expect ...
#
# Individual source names may contain paths relative to the testcase "src"
# subdirectories (local and uptree). e.g.:
#
#   -- # subdir1/p.adb
#
# This allows locating sources when they aren't straight within src/, which is
# useful e.g. for tests of GPR facilities. This doesn't influence how we
# expect the source to be referenced in reports, however: we still match with
# basename in =report slocs (p.adb:128:15) or in annotated source file names
# (p.adb.xcov).
#
# A '+' prefix allows stating that we expect the subdirs to be visible in the
# report references (slocs or filenames), necessary for tests involving
# sources with identical basenames in different subdirs. Currently, this turns
# '/' into '-' in annotated source filenames, so +subdir1/p.adb is expected to
# produce subdir1-p.adb.xcov reports.
#
# Only '/' is allowed as the directory separator in this context.

# LINE REGULAR EXPRESSION (LREs)
# ------------------------------
# LX_LRE in our grammar is a regular expression that is used to identify
# source lines that match "-- # " + lx_lre.  We decided to make the "-- # "
# implicit in order to reduce the lx_lre expression to a minimum as well as
# to force a standard format for all such markers.
#
# Slashes inside lx_lre tokens are allowed. The SCOV_data parser simply
# uses the first and last slash as the delimiters.
#
# Having two distinct LX lines with the same LRE is forbidden and flagged as
# an error unless the newer LRE is marked as "overriding" with the optional
# "=" prefix, in which case the corresponding LX line overrides the current
# one for that LRE.
#
# The following construct is forbidden and will cause a test failure:
#
# -- /blu/ l- ## s-
# -- /blu/ l- ## c!:"A"
#
# The following one is allowed and yields the second set for /blu/:
#
# --  /blu/ l- ## s-
# -- =/blu/ l- ## c!:"A"

# RNOTE SUBTEXT FOCUS/SPECIALIZATION (:"TEXT" extensions)
# -------------------------------------------------------
# The optional :"TEXT" part in a rnote lets users state that some diagnostic
# is expected to designate a particular piece of a source line. This is most
# typically useful for c! expectations on lines where there are multiple
# conditions, and users want to state the particular condition on which a c!
# diagnostic is expected.
#
# For example, with a piece of functional code like
#
#     if A and then B then -- # eval
#
# An mcdc test driver which exercises A correctly and not B can state
# expectations like:
#
#  -- /eval/ l! ## c!:"B"
#
# The quoted "TEXT" is interpreted as designating the corresponding section of
# each source line matched by the LRE, in a case-sensitive manner. For example,
# "Operand_A" in :
#
#  -- /eval/ l! ## c!:"Operand_A"
#
# will designate columns 4 to 11 on line 3 and columns 15 to 22 on line 9
#
#        4      11
# ...    v       v
#  3: if Operand_A and then Operand_B then -- # eval
#  ...
#  9: X := K or else Operand_A;     -- # eval
#                    ^       ^
#                    15     22
#
# The special form "(TEXT*)" is also recognized to designate the full text
# included in the outer parens plus the parens themselves. This is useful
# to designate complete parenthesised operands in situations like:
#
#   if (x < 12) && (y > 30)  // # eval
#
# As the slocs designated by gnatcov are not necessarily the start of the
# conditions but could point at the root expression operators instead.
#
# In this example, gnatcov could well point at '<' or '>' to designate a
# partially covered condition, and specifying c!:"x" or c!:"y" wouldn't
# work. c!:"(x*)" and c!:"(y*)" could be used instead.
#
# Apart from this special form, an exact matching string is expected.

# CONTINUATION LINES (CONT lines)
# -------------------------------
# Are meant to allow adding rnotes to a just preceding LX line. This is
# typically useful when STAGs and generics are involved as this often leads
# to multiple expectations with longuish descriptions. For example;
#
# -- %cov: -S routine %cargs: -O1, -gnatn
# --  =/test-out/  l! ## s-@(sensors__rf__test), s-@(sensors__rc__test)
# --                  +# c!:"Low"@(sensors__test)
#                     ^^
#                     continuation here

# SEPARATION TAGS (STAG addends)
# ------------------------------
# An STAG addend, when specified, is the separation tag expected for the
# note. This will be a bare routine name for outputs with -S routines, and a
# nest of instantiation slocs for outputs with -S instances. In the latter
# case, slocs are stated in a symbolic manner, as "i:NAME" to denote a generic
# instantiation somewhere on a source line featuring an "i:NAME" anchor, for
# any source part of any of the groups.
#
# In absence of STAG part, the corresponding emitted notes must not contain
# any separation tag indication to match. A bare "s-" is only matched by a
# bare "statement not executed" =report indication for example, without any
# separation tag mentioned.

# CONDITIONAL EXPECTATIONS WITHIN A GROUP (CTL lines)
# ---------------------------------------------------
# %cargs: opt1[, opt2, ... optn] means: from now on, only grab the next-coming
# lx lines if opt1 (and opt2 and ... up to optn) are part of the compilation
# options for the test. A '!' at the beginning of an option inverts the logic
# for this option in particular, meaning "... if this option is *not* part of
# of the compilation args for the test.
#
# %cov:  ... is similar, against the gnatcov coverage options for the test,
#            instead of the compilation flags.
#
# %tags: ... is similar, against the testsuite discriminants, of particular
#            interest for a possible toolchain indication.
#
# %opts: ... is similar, against options passed on the test execution command
#            line, possibly coming from the upper testsuite level.
#
# When some of the conditions don't match, lx lines are ignored until
# the decision to start grabbing again triggers from a subsequent CTL line.
#
# The CTL lines are just grabbing/not-grabbing frontiers. They don't influence
# the current state of the expectation group otherwise. In particular, the set
# of already recognized LREs remains the same so an overriding indication is
# required if you wish an alternate specification.
#
# Here is a simple example:
#
# --# p.adb                         <= Start expectation group wrt p.adb
# --  /bla/ ...
# --
# --  /blu/ l! ## s-                <= By default (about to be conditionally
#                                      overriden), expect no separation tag
#                                      on s- for /blu/.
#
# --%cov: -S instances              <= From now on, grab expectations only if
#                                      coverage -S instances. When so ...
# --  =/blu/ l! ## s-@(i:myinst)    <= Override expectations for /blu/
#
# --%cov: -S routines               <= Likewise for coverage -S routines
# --  =/blu/ l! ## s-@(myinst__blu)
#
# Conditioning on cargs works the same. Both kinds of controls appearing on
# the same line are and-combined each individual control may be inverted with
# a '!' prefix. For example:
#
# -- From now on, grab expectations only if gnatcov coverage -S routines
# -- and compilation without -gnatn and with -gnatp:
#
# --%cov: -S routines  %cargs: !-gnatn, -gnatp
#
# Note that contrary to STAGS where absence in expectation means we expect no
# emitted STAG at all, absence of an option in a CTL list means that it
# doesn't matter whether this option is actually passed or not. '!' is there
# for cases where we need to state dependency on option absence.
#
# The user control values are taken as regular expressions against which
# the actual arguments are matched. This is convenient for sections conditioned
# on --level for instance, where mcdc and uc_mcdc are most often identical
# and really deserve common treatment.
#
# --%cov: --level=stmt
# ...
# --%cov: --level=stmt+decision
# ...
# --%cov: --level=stmt+(uc_)?mcdc
# ...

# "%cancel:" means "forget the current group", IOW, "ignore anything emitted
# for the corresponding sources" if all the previous conditions are met. This
# is useful for very specific combinations of compilation options and coverage
# analysis modes known to be meaningless, such as -gnatn inlining together
# with -S routine, expressed like
#
#   %cov: -S routine  %cargs: -gnatn  %cancel:

# "%cons:" allows filtering on possible alternative consolidation modes,
# with "traces" or "checkpoints like:
#
#   drivers=_overlap|_no_overlap
#
#   --# ranges.adb
#   --  %cons: traces
#   --  /check/  l+ ## 0
#
#   --  %cons: checkpoints
#   --  /check/  l! ## dT-

# We use three intermediate abstractions to build the dictionaries from
# the expectations text:
#
# * Line Coverage eXpectations (LineCX) objects, to represent individual
#   expectations line like  "--  /bla/ l- ## s-", and
#
# * Unit Coverage eXpecations (UnitCX) objects to represent the associations
#   of a sequence of line expectations with a single unit name.
#
# * Unit eXpectation Groups (UXgroup) objects to represent the associations of
#   a sequence of line expectations with a list of unit names.
#
# For example, assuming an Ada driver, the excerpt below ...
#
# --# p1.adb p2.adb
# --  /bla/ l- ## s-
# --  /blo/ l+ ## 0
#
# --# x.adb
# --  /blu/ l+ ## 0
#
# Will first yield:
#
# * One UCX group for [p1.adb, p2.adb] associated with
#   two LineCX objects, one for /bla/ and one for /blo/
#
# * One UCX group for [x.adb] associated with one LineCX
#   object for /blu/
#
# From there, we'll eventually produce three UnitCX objects:
#
# * One instantating internal note objects for /bla/ + /blo/ from p1.adb
#
# * One instantating internal note objects for /bla/ + /blo/ from p2.adb
#
# * One instantating internal note objects for /blu/ from x.adb


class Sref:
    """
    Source reference class, materializing source names expressed in
    expectations.
    """

    def __resolve(self, xpath):
        """
        Return a valid relative path were the source designated by XPATH in the
        expectations may be found, searching plausible locations uptree from
        the current point.
        """

        for pdir in ("../" * n + "src/" for n in range(0, thistest.depth)):
            if os.path.exists(pdir + xpath):
                return pdir + xpath

        return None

    def __init__(self, xsource):
        """
        Materialize the XSOURCE indication provided for an expectation
        group.
        """

        self.xsource = xsource

        # XPATH: The relative path expressed in the expectation:
        self.xpath = xsource[1:] if xsource.startswith("+") else xsource

        # SPATH: The resolved path to an actual source file reachable for
        # the testcase at hand:
        self.spath = self.__resolve(self.xpath)


class LineCX:
    """
    Line Coverage eXpectations class.  Associated with source file names, these
    define unit coverage expectation specs.
    """

    def __init__(self, lre, lnp, rnps, override):
        self.lre = lre
        self.lnp = lnp
        self.rnps = rnps

        # Whether this line is meant to override an existing one
        # with an identical lre:

        self.override = override

    def instanciate_lnotes_over(self, tline, block, srules):
        return [self.lnp.instanciate_over(tline, block, srules)]

    def instanciate_rnotes_over(self, tline, block, srules):
        return [
            rnp.instanciate_over(tline, block, srules)
            for rnp in self.rnps
            if rnp.kind
        ]


class UnitCX:
    """
    Associate a single source name with a list of expected Coverage Line
    eXpectations. Construct the corresponding Line and Report Xnote
    dictionaries.
    """

    # Expected notes instanciations

    def instanciate_notes_for(self, lx, tline, block, srules):
        for ln in lx.instanciate_lnotes_over(tline, block, srules):
            if ln:
                self.xldict.register(ln)
        for rn in lx.instanciate_rnotes_over(tline, block, srules):
            if rn:
                self.xrdict.register(rn)

    # Kind subsitution rules processing
    #
    # For shared drivers that exercise boolean expressions in different
    # possible contexts (control constructs or others), determined by the
    # functional source. For example,
    #
    #    Assert (Andthen (A => True, B => True));
    #
    # doesn't exercise the False outcome of an and-then expression, and
    # the way this will be reported depends on the context when the and-then
    # expression appears.
    #
    # A shared driver would express this with "oF-", that needs to be turned
    # into, say, dF- or eF- according to hints in the functional source.  Such
    # hints are provided as :<subst-key>,<subst-key>,...: at the end of line
    # anchors, with the following possible values for each <subst-key>:

    subst_tuples_for = {
        # o/d: Outcome expectations for line are to produce "decision"
        # expectations, for instance on
        #
        #     if A and then B then  -- # eval :o/d:
        "o/d": {
            otNoCov: dtNoCov,
            ofNoCov: dfNoCov,
            oPartCov: dPartCov,
            oNoCov: dNoCov,
        },
        # o/e: Outcome expectations for line are to produce "expression"
        # expectations, for instance on
        #
        #    return Value (A and then B); -- # eval :o/e:
        "o/e": {
            otNoCov: etNoCov,
            ofNoCov: efNoCov,
            oPartCov: ePartCov,
            oNoCov: eNoCov,
        },
        # o/0: outcome expectations for line are to be ignored, for
        # contexts where the evaluated expression is not actually a
        # decision, for instance on
        #
        #    pragma Precondition (not X); -- # eval :o/0:
        "o/0": {
            otNoCov: None,
            ofNoCov: None,
            oPartCov: None,
            oNoCov: None,
            lPartCov: None,
        },
        # s/e: Statement uncovered expectations for line are to be
        # matched as expression never evaluated, for contexts where
        # the evaluated expression is not enclosed in a statement,
        # for instance on
        #
        #    pragma Precondition (A and then B); -- # eval :s/e:
        "s/e": {sNoCov: eNoCov},
        # eval on the line are in expression or decision context
        "e": {},
        "d": {},
    }

    def check_srules_on(self, tline):
        # Check for kind substitution rules on this line. Reset
        # at every line for now.

        m = re.search("# .*:(.*):$", tline.text)

        if not m:
            self.current_srules = None
        else:
            self.current_srules = {}
            for sim in m.group(1).split(","):
                self.current_srules.update(self.subst_tuples_for[sim])

    # Toplevel processing

    def process_tline(self, tline):
        self.check_srules_on(tline)
        for lx in self.LXset:
            if re.search(lx.lre, tline.text):
                self.instanciate_notes_for(
                    lx, tline, self.current_block, self.current_srules
                )

    def __init__(self, sref, LXset):
        self.LXset = LXset

        # dictionaries of expected line and report notes for our unit

        self.xldict = KnoteDict(xlNoteKinds)
        self.xrdict = KnoteDict(xrNoteKinds)

        self.current_block = None
        self.current_srules = {}

        self.tfile = Tfile(filename=sref.spath, process=self.process_tline)

        self.sref = sref

        thistest.stop_if(
            self.current_block, FatalError("fuzz block still open at EOF")
        )


class UXgroup:
    def __init__(self, candlists):
        # SRLIST: good list of source ref objects from the set of candidate
        # lists received in CANDLISTS for this group, as specified in the
        # expectation spec. This is a list of lists like
        #
        #   [[x0.adb, y0.adb], [x1.c, y1.c]]
        #
        # where
        #
        # - each sublist is a set of sources to which the set of line
        #   expectations should attach.
        #
        # - exactly one sublist is expected to correspond to sources
        #   we can actually find, which will be _the_ good one.

        self.srlist = self.__select_srlist_from(candlists=candlists)

        # LXSET: During parsing, a dictionary of LineCX objects corresponding
        # to the stated expectations for the sources we can find, indexed by
        # LRE so we can easily override entries when processing conditional
        # sections.  This is turned into a list eventually, when we know the
        # set of values is fixed and all we need is iterate over values.

        self.lxset = {}

        # UXSET: a list of UnitCX instances, one per unit in the single
        # good list in the CANDLISTS candidates. Computed on close().

        self.uxset = None

    # Helpers for __init__

    def __examine_source_list(self, slist, goodlists):
        """
        See if all the sources in SLIST can be resolved to existing source
        paths looking uptree. Add the corresponding list of paths to GOODLISTS
        when so.
        """

        srlist = []
        for s in slist:
            sr = Sref(xsource=s)
            if not sr.spath:
                return
            else:
                srlist.append(sr)

        goodlists.append(srlist)

    def __select_srlist_from(self, candlists):
        """
        Search and return the one good list of units amongst the candidates we
        have.
        """

        goodlists = []
        for slist in candlists:
            self.__examine_source_list(slist, goodlists)

        thistest.stop_if(
            len(goodlists) != 1,
            FatalError(
                "goodlists = %d, != 1 for %s" % (len(goodlists), candlists)
            ),
        )

        return goodlists[0]

    # Helpers for close()

    def __wrap_lre(self, lx, langinfo):
        """
        For a source expressed in the language described by LANGINFO, adjust
        line regular expression in LX to expect it prefixed with "xx # " where
        "xx" is the language comment marker.
        """

        lx.lre = langinfo.comment + " # (" + lx.lre + ")"

        # The parens are crucial here. Consider what would happen for
        # /bla|blo/ without them ...

    def close(self):
        """
        For each valid unit designated by one of our candidate lists,
        instantiate a UnitCX object and latch the list of instances.
        """

        # Wrap LREs to make sure we look for them in explicit anchors within
        # sources, not as arbitrary sections of source lines. The way to do
        # this depends on the source languages. We assume they are all the
        # same for our list.

        for lx in self.lxset:
            self.__wrap_lre(lx, language_info(self.srlist[0].xpath))

        # Now instanciate a unit coverage expectations object for each
        # source ref in our list:

        self.uxset = [
            UnitCX(sref=sref, LXset=self.lxset) for sref in self.srlist
        ]

        return self.uxset


class XnotesExpander:
    """
    We need to parse things slightly differently for different languages. For
    example, expectation lines or expectation anchors which start with a
    comment marker.

    The comment marker for expectation lines depends on the language where the
    expectation lines are found, which migh be a test driver or a consolidation
    spec).

    The comment marker for expectation anchors depends on the language of the
    unit where this anchor will be matched.

    For example, a multi language expectations spec in an Ada driver or a
    consolidation spec would look like:

      comment marker for an Ada test driver or a consolidation spec
      v
      --# foo.adb
      --  /foo-ref/  ...  (search for "-- # foo-ref" in foo.adb)
                                       ^ comment marker for Ada
      --# bar.c
      --  /bar-ref/  ...  (search for "// # bar-ref" in bar.c)
                                       ^ comment marker for C

    In a C test driver, the leading comment marker would be "//".
    """

    # __get_scovdata ()
    #
    # __parse_scovdata ()
    #   >  __parse_groups_from ()
    #   |   > __try_ctl_update_from ()
    #   |   > __parse_sources ()
    #   |   > __parse_lcx ()
    #   |
    #   >  __resolve_stags_from ()
    #   |   > __resolve_itags_from ()
    #   |   > xnp.instantiate_stag () for all xnote patterns
    #   |
    #   >  g.close () for all parsed groups

    def __init__(
        self,
        xfile,
        xcov_level,
        ctl_opts,
        ctl_cov,
        ctl_cargs,
        ctl_tags,
        ctl_cons,
    ):
        # XFILE is the name of the file from which coverage expectations
        # are to be extracted.

        self.xfile = xfile

        # XCOV_LEVEL is the --level argument we are going to use.
        # This is useful here to help select expectations when some
        # are conditioned on levels ("m=>c!..." etc).

        self.xcov_level = xcov_level

        # CTL_OPTS, CTL_COV, CTL_CARGS, CTL_TAGS and CTL_CONS are the reference
        # lists of control strings for CTL lines:
        #
        # CTL_OPTS  - test option strings of interest (e.g. "trace_mode=src"),
        # CTL_COV   - list of gnatcov coverage options that we are going to use
        # CTL_CARGS - compilation options
        # CTL_TAGS  - set of discriminants for the current run, and
        # CTL_CONS  - kinds of consolidation artifacts we are requested to use

        self.ctls = {
            "%opts": " ".join(ctl_opts),
            "%cargs": " ".join(ctl_cargs),
            "%cov": " ".join(ctl_cov),
            "%tags": " ".join(ctl_tags),
            "%cons": " ".join(ctl_cons),
        }

        # And these are the dictionaries we expose. This includes the expected
        # line notes, the expected report notes, and the absolute paths to the
        # source for each source referenced in expectations:

        self.xlnotes = {}
        self.xrnotes = {}
        self.abspaths = {}

        for ux in self.__parse_scovdata(self.__get_scovdata(xfile)):
            self.__expose(ux)

    def __expose(self, ux):
        # A '+' prefix on the source reference means we expect
        # sources to be referenced with relative dir indications:

        source = (
            ux.sref.xpath
            if ux.sref.xsource.startswith("+")
            else os.path.basename(ux.sref.xpath)
        )

        self.abspaths[source] = os.path.abspath(ux.sref.spath)
        self.xrnotes[source] = ux.xrdict
        self.xlnotes[source] = ux.xldict

    def __get_scovdata(self, scov_file):
        """
        Return a list of strings containing the SCOV_data.  To simplify
        parsing, the leading comment markers are stripped.
        """

        # The langinfo corresponding to the language of SCOV_FILE
        lang_info = language_info(scov_file)

        # The scov data begins at the first line that starts with the
        # language's comment marker, followed by a '#'. Any line that
        # starts as a comment after this first '#' comment line is assumed
        # to be part of the scov data.  Build a list of lines containing
        # the scov data stored in scov_file now.
        contents = []
        in_scovdata = False
        for line in lines_of(scov_file):
            # Take care of leading/trailing spaces to give the user
            # more flexibility.  Also take care of the trailing new-line
            # character that we get from lines_of.
            line.strip()
            if line.startswith(lang_info.comment + "#"):
                in_scovdata = True
            if in_scovdata and line.startswith(lang_info.comment):
                # Also take this opportunity to strip the leading comment
                # string as well as any space immediately following it.
                # This will simplify the parsing a little bit.
                contents.append(line[len(lang_info.comment) :].lstrip())
        return contents

    def __parse_scovdata(self, scovdata):
        """
        Parse the given SCOVDATA lines and return the corresponding list of UCX
        instances.
        """

        uxgroups = self.__parse_groups_from(scovdata)

        # At this point, the groups aren't closed yet and we have something
        # like:
        #
        # uxgroups = [UXgroup (), UXgroup () ...]
        #             v
        #            ---------
        #            .srlist = ["p1.adb", "subdir/p2.adb", ...]
        #            .lxset  = [LineCX (), LineCX (), ...]
        #            .           v
        #            .        -------
        #            .        .lre  = "/bla/"
        #            .        .lnp  = XnoteP (...)
        #            .        .rnps = [XnoteP (), XnoteP (), ...]
        #            .                 v
        #            .               --------
        #            .               .kind  = sNoCov
        #            .               .stext = ...
        #            .               .stag  = "<some stag>"
        #            .
        #            .uxset = None

        # Before closing the groups to trigger the UnitCX creations together
        # with the note instantiations from the XnoteP patterns, we run a name
        # to sloc resolution pass for stags

        self.__resolve_stags_from(uxgroups)

        # Now we can close the groups and construct the complete list
        # of UnitCX instances.

        return [ux for uxg in uxgroups for ux in uxg.close()]

    # __parse_groups_from() helper for __parse_scov_data

    def __parse_groups_from(self, scovdata):
        """
        First level of group parsing, stopping prior to XnoteP instantiations
        to allow name -> sloc resolution in between.
        """

        uxgroups = []

        # We start a new group everytime we see a "sources" line (which starts
        # with '#', after comment markers were stripped).

        current_uxg = None

        grabbing = True

        # Track the last LCX we grabbed, so we can process continuation
        # requests

        lastlx = None

        for line in scovdata:
            ctl_update, ctl_value = self.__try_ctl_update_from(line)

            if ctl_update:
                # A CTL line was found, update our processing state
                # accordingly:

                if ctl_value is None:
                    current_uxg = None
                else:
                    grabbing = ctl_value

            elif grabbing and line.startswith("+#"):
                # A continuation line, to add rnotes that didn't fit
                # on the previous ones.

                lastlx.rnps.extend(self.__parse_expected_rnotes(line[3:]))

            elif line.startswith("#"):
                # A new group starts. Close the current one first and start
                # grabbing again unconditionally:

                if current_uxg is not None:
                    uxgroups.append(self.__end_parse_on(current_uxg))

                current_uxg = UXgroup(candlists=self.__parse_sources(line))
                grabbing = True

            elif grabbing and line.startswith(("/", "=")):
                # This must be an LX line. Check lre overriding and add to the
                # set attached to the current group consistency.

                lx = self.__parse_lcx(line)

                thistest.stop_if(
                    lx.lre in current_uxg.lxset and not lx.override,
                    FatalError(
                        "LRE dup without overriding note (%s, %s)"
                        % (lx.lre, self.xfile)
                    ),
                )

                current_uxg.lxset[lx.lre] = lx
                lastlx = lx

            else:
                # Not grabbing or regular comment. Just ignore.
                pass

        # We're done with all the lines. Close the current group, if any

        if current_uxg is not None:
            uxgroups.append(self.__end_parse_on(current_uxg))

        return uxgroups

    # __end_parse_on() helper for __parse_groups_from()

    def __end_parse_on(self, uxg):
        """
        Called whe the first level parsing is done for UXG, prior to instance
        name resolution and group closing.
        """
        uxg.lxset = list(uxg.lxset.values()) + self.__builtin_lcxs_for(uxg)
        return uxg

    # Builtin markers support: to let test writers put things like
    #
    #    -- # __l-s-
    #
    # in the functional sources to mean: unless explicitly overriden by
    # a regular expectation spec, expect l- s- for this line always.
    #
    # What default notes we expect for what designator text

    builtin_lxs = {
        "__l-s-": "l- ## s-",
        "__l!d!": "l! ## d!",
        "__l!dT-": "l! ## dT-",
    }

    def __builtin_lcxs_for(self, uxg):
        """Add builtin default LineCX for UXG."""

        # Fetch the explicit line expectations and compute those not there for
        # which we have a default to provide. Beware that the expressions were
        # wrapped by parse_lcx already

        nothere = [lre for lre in self.builtin_lxs if lre not in uxg.lxset]

        # Now compute the list of LCX objects for each of those defaults

        return [
            self.__parse_lcx("/%s/ %s" % (lre, self.builtin_lxs[lre]))
            for lre in nothere
        ]

    # CTL parsing for control of conditional sections

    def __try_ctl_update_from(self, line):
        """
        See if LINE is a CTL line and return the corresponding (do_update,
        now_active) indication tuple. do_update tells if indeed LINE is a CTL
        line and now_active tells what to do next: True means "all the
        filtering conditions were matched against the current set of active
        ones, so grab whatever comes next for the group until the next CTL".
        False means "Some conditions were not met, stop grabbing for the
        current group until the next CTL". None means "A conditional request
        for group nullification was expressed and all the conditions were
        satisfied, so just forget the current group".
        """

        # CTL lines are like "%cov: -S routines %cargs: !-gnatn"
        # They are the only lines that may start with '%'.

        if not line.startswith("%"):
            return (False, None)

        # To keep grabbing LX lines, all the CTL parts must evaluate True.
        # Shortcircuit on match failure so we know that evaluation still
        # running at one point means that all the conditions evaluated so far
        # were satisfied.

        parts = re.findall(pattern="%[^%\n]*", string=line)

        for part in parts:
            this_val = self.__eval_ctl_update_from(part)

            if this_val is None:
                # Nullification request. Still evaluating here so we know
                # that all the prerequisites were satisfied:
                return (True, None)

            elif not this_val:
                # This piece didn't match, shortcircuit:
                return (True, False)

            else:
                # This piece matched and was not a cancellation request,
                # keep evaluating:
                continue

        # All the pieces matched and there was no cancellation request
        # on the way:

        return (True, True)

    def __eval_ctl_update_from(self, part):
        """
        PART is a piece of CTL line for a single specific key, such as "%cov:
        -S instance, --level=stmt", "%cargs: !-gnatn", "%tags: 7.0.3" or
        "%cancel:".

        If this is "%cancel:", return None to reflect the nullification intent
        for the control. Otherwise, evaluate and return whether all the
        elements are in our current CTL references (set of actual coverage or
        compilation options provided at init time), in whatever order. In the
        first example just quoted, this will return True iif "-S instance" and
        "--level=stmt" are both in the actual coverage options, in this order
        or the other.
        """

        # First fetch the key and the set of option sequences to match:

        m = re.match(pattern=" *(?P<key>%.*?):(?P<opts>.*)\n?", string=part)

        key = m.group("key")
        opts = m.group("opts").strip()

        # If this is a nullification request, notify so:

        if key == "%cancel":
            return None

        # Now evaluate whether each sequence is in the actual set
        # of options that we were given for the key:

        this_val = True
        for oseq in opts.split(","):
            oseq = oseq.strip()
            if oseq.startswith("!"):
                invert = True
                oseq = oseq[1:]
            else:
                invert = False

            optin = re.search(pattern=oseq, string=self.ctls[key]) is not None

            this_val &= not optin if invert else optin

        return this_val

    # __parse_sources() helper for __parse_groups_from

    def __parse_sources(self, image):
        """
        Given IMAGE as a string that contains a "sources" line, parse that line
        and return a list of lists, one for each possible set of sources
        expected to apply.
        """

        # # x0.adb y0.adb | x1.c y1.c
        #
        # -> [["x0.adb", "y0.adb"], ["x1.c", "y1.c"]]
        #
        # to mean "this section applies to (x0.adb and y0.adb) or (x1.c an
        # y1.c), whichever set we can reach from here.

        return [alt.split(" ") for alt in image[1:].strip().split("|")]

    # Instance name resolution helpers for __resolve_stags_from()

    # Instance names resolution to file:line kind of slocs.
    #
    # In s-@(i:NAME), "i:NAME" resolves to "<file>:<line>" when we
    # have ...
    #
    #          <file>
    #          ...
    # <line>:  <instanciation code here>  -- # i:NAME
    #                                        ^^^^^^^^
    #                                 instanciation marker here

    # The real *source instance* designation marker, that disambiguates
    # separation tags out of -S instances from routine separation tags out of
    # -S routines in expectations, and which we also expect in instantiation
    # line anchors.

    imark = "i:"

    def __resolve_itags_within(self, xnp, idict):
        # xnp.stag contains something like i:NAME1[i:NAME2[i:NAME3]] to
        # designate instantiations

        def __sloc_for(m):
            name = m.group(0)

            # We expect exactly one match for a name so could arrange to stop
            # at the first we find. Having multiple matches can happen out of
            # a test-writer's mistake though, and keeping only one arbitrarily
            # would cause endless confusion so we search them all and issue an
            # error as needed.

            # We expect to compare only against line numbers later on, so just
            # stash a dummy column number here, required to form a regular
            # Sloc still.

            slocs = [
                "%s:%d:0" % (os.path.basename(sp), tl.lno)
                for sp in idict
                for tl in idict[sp]
                if name in tl.text
            ]

            thistest.stop_if(
                len(slocs) != 1,
                FatalError("%d slocs found for stag %s" % (len(slocs), name)),
            )

            return slocs[0]

        xnp.stag = re.sub(
            pattern="%s[A-Za-z_0-9]+" % self.imark,
            repl=__sloc_for,
            string=xnp.stag,
        )

    def __resolve_itags_from(self, all_xnps, uxgroups):
        """
        Resolve references like "i:NAME" in stags into the file:line sloc where
        an instantiation of NAME is located in the set of sources covered by
        all the units in UXGROUPS.
        """

        i_xnps = [
            xnp for xnp in all_xnps if xnp.stag and self.imark in xnp.stag
        ]

        if len(i_xnps) == 0:
            return

        # Fetch instantiation lines from sources and resolve.  We expect
        # exactly one instantiation per tagged line.

        def __ilines_for(sp):
            tf = Tfile(filename=sp, process=lambda tl: None)
            return [
                tl for tl in tf.contents() if "# %s" % self.imark in tl.text
            ]

        spaths = [sref.spath for uxg in uxgroups for sref in uxg.srlist]
        idict = {sp: __ilines_for(sp) for sp in spaths}

        for xnp in i_xnps:
            self.__resolve_itags_within(xnp=xnp, idict=idict)

    # __resolve_stags_from() helper for __parse_scovdata

    def __resolve_stags_from(self, uxgroups):
        """
        Resolve instance references in separation tags from UXGROUPS, then turn
        separation tag strings into Stag objects.
        """

        all_xnps = [
            xnp for uxg in uxgroups for lx in uxg.lxset for xnp in lx.rnps
        ]

        self.__resolve_itags_from(all_xnps=all_xnps, uxgroups=uxgroups)

        for xnp in all_xnps:
            if xnp.stag is not None:
                xnp.instantiate_stag()

    # Note parsing and selection helpers for __parse_lcx

    def __parse_expected_rnotes(self, image):
        # We will use shlex (which is useful for parsing quoted strings).
        # It enables us to split the given string, according a given separator,
        # but still preserves quoted text altogether (i.e. if there is a
        # separator in some quoted text, it won't be considered as a
        # separator).
        #
        # The separator can be a "#", or a ",", so we will split on them. We
        # will also remove whitespaces (as when splitting, shlex also skips
        # the separator characters). This won't be a problem, as quotes will
        # protect the only spaces we care about.
        #
        # Also note that shlex also removes the quotes, so if we pass it the
        # following string:
        # c!:"B, " # d!:"C and then D"
        # Assuming # is a separator, we will get as a result of splitting:
        # ['c!:B, ', 'd!:C and then D']

        splitter = shlex.shlex(image, posix=True)
        splitter.whitespace_split = True
        splitter.whitespace += "#,"
        imlist = list(splitter)
        return [
            rnote
            for rnote in (self.__parse_one_expected_rnote(im) for im in imlist)
            if rnote
        ]

    def __parse_one_expected_rnote(self, image):
        # We have at hand single note spec, possibly conditioned by the
        # xcov-level. Something like "s-", "d=>dT-", or "mu=>c!:"B".

        # We might also have an expected separation tag in any of these
        # cases, e.g. c!:"B"@(my_instance)

        # First fetch the note text that corresponds to our actual xcov-level.
        # If we have no applicable text, state so.

        ntext = self.__select_rnote(image)
        if not ntext:
            return None

        # Otherwise, figure out the note kind + possible column localization +
        # possible instance specialization parts and return a note pattern
        # instance

        if "@(" in ntext:
            ntext, stag = ntext.split("@(")
            stag = stag.rstrip(")")
        else:
            stag = None

        if ":" in ntext:
            # We split only once, as we have occurrences of notes as such:
            # s-:"P :=" (the string as parsed by shlex would be 's-:P :='),
            # with a colon inside the quoted string. We don't want to split on
            # that second colon, as it is part of the source code.
            ntext, stext = ntext.split(":", 1)
        else:
            stext = None

        return XnoteP(text=ntext, stext=stext, stag=stag)

    def __decode_note_choice(self, text):
        """
        Given a note_choice that depends potentially on a list of coverage
        levels, return a list of (xcov-level, expected-note-text) tuples that
        represent those dependences.

        For instance, given:

            'u => l!' or 'sd => l+'

        ...this function will return:

            [('stmt+uc_mcdc', 'l!')]
            or
            [('stmt', 'l+'), ('stmt+decision', 'l+')]
        """
        level_from_char = {
            "s": ["stmt"],
            "d": ["stmt+decision"],
            "m": ["stmt+mcdc"],
            "u": ["stmt+uc_mcdc"],
        }

        def make_new_lvl_combinaison(lvl):
            """
            Add the coverage level lvl to other regular coverage level
            combinaisons defined in level_from_char. Return the list of
            combinaisons.
            """
            res = []
            for char in level_from_char:
                for comb in level_from_char[char]:
                    res.append(comb + "+" + lvl)
            return res

        assert_level_from_char = {
            "a": make_new_lvl_combinaison("atc"),
            "c": make_new_lvl_combinaison("atcc"),
        }

        level_from_char.update(assert_level_from_char)

        fun_call_level_from_char = {
            "f": make_new_lvl_combinaison("fun_call"),
        }

        level_from_char.update(fun_call_level_from_char)

        result = text.split("=>")

        if len(result) == 1:
            # No choice
            return [("", text)]
        elif len(result) > 2:
            # Parse error
            raise FatalError(
                "Note choice %s contains more than one arrow" % text
            )
        else:
            note = result[1].lstrip(" ")
            lev_list = result[0].rstrip(" ")

            res = []
            for lchar in lev_list:
                for combinaison in level_from_char[lchar]:
                    res.append((combinaison, note))

            return res

    def __select_lnote(self, text):
        """Decode text to return the line note for the current
        coverage level."""

        lx_lnote_list = [alt.strip() for alt in text.split(",")]

        level_table = dict(
            ln_tuple
            for cond_notes in lx_lnote_list
            for ln_tuple in self.__decode_note_choice(cond_notes)
        )

        if self.xcov_level in level_table:
            return level_table[self.xcov_level]
        elif "" in level_table:
            return level_table[""]
        else:
            raise FatalError(
                "Missing line expectation choice for level %s in %s"
                % (self.xcov_level, text)
            )

    def __select_rnote(self, text):
        """Decode TEXT into a report note for the current coverage level."""

        # Set of level->note_kind associations in TEXT

        level_table = dict(
            ln_tuple for ln_tuple in self.__decode_note_choice(text)
        )

        # If we have one association for the exact level we're running,
        # use that. Fallback to a default kind if we have one.

        if self.xcov_level in level_table:
            return level_table[self.xcov_level]
        elif "" in level_table:
            return level_table[""]

        return None

    def __parse_lcx(self, image):
        """
        Parse IMAGE as a string that contains a line expectation spec and
        return the corresponding LineCX object.
        """

        # Extract the various parts of interest from the image

        m = re.match(
            string=image,
            pattern=r"\s*(?P<lre>=?/.*?/)\s+(?P<lnote>.*) ## (?P<rnotes>.*)",
        )

        if m is None:
            raise FatalError(
                "Invalid '%s' line expectation spec."
                "\nExpected /LRE/ lnotes ## rnotes" % image
            )

        lx_lre = m.group("lre")

        lre_override = lx_lre[0] == "="
        if lx_lre[0] != "/":
            lx_lre = lx_lre[1:]

        lx_lre = lx_lre.strip("/")

        lx_lnote = XnoteP(
            text=self.__select_lnote(m.group("lnote")), stext=None
        )

        lx_rnotes = self.__parse_expected_rnotes(m.group("rnotes"))

        # If none of the stated report expectation applies to the current
        # xcov-level, default to the empty set:

        if not lx_rnotes:
            lx_rnotes = [XnoteP(text="0", stext=None)]

        # If we have both an empty-set expectation and something else,
        # expectations are wrong. This is a safeguard against a common
        # mistake, thinking, say, that "d=>dT-, 0" means "dT- if
        # --level=stmt+decision, 0 _otherwise_", while it means "dT- etc, 0
        # _always_ (not tied to any particular level)" instead.

        else:
            thistest.stop_if(
                len(lx_rnotes) > 1 and "0" in lx_rnotes,
                FatalError("Contradictory =report expectation in %s" % image),
            )

        return LineCX(
            lre=lx_lre, lnp=lx_lnote, rnps=lx_rnotes, override=lre_override
        )
