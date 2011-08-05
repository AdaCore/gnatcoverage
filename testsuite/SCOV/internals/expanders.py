# ****************************************************************************
# **                              NOTE EXPANDERS                            **
# ****************************************************************************

# Expose facilities to construct per-unit dictionaries of expected or reported
# coverage notes, extracted from text files (=xcov or =report outputs, driver
# sources).

# The result dictionary keys are source names and values are KnoteDict
# objects (per kind dictionary of note instances)
#
# { [sourcename] -> { [note kind] -> [ Cnote, Cnote, ... ],
#                     ...
#                   },
#   ...
# }

# Below is a rough sketch of the entities and classes involved:
#
#   test_blob1.adb       blob1.adb
#   ...                      v
#   Expect.Patterns   >  XnotesExpander
#     (XnoteP)               v
#                 .xlnotes = { sourcename - KnoteDict(lNoteKinds) of Xnote }
#                 .xrnotes = { sourcename - KnoteDict(rNoteKinds) of Xnote }
#
# xcov --annotate=xcov   -> *.xcov
#                            v LnotesExpander
#                  .elnotes = { sourcename - KnoteDict(lNoteKinds) of Enote }
#
# xcov --annotate=report -> test.rep
#                            v RnotesExpander
#                  .ernotes = { sourcename - KnoteDict(rNoteKinds) of Enote }

# ****************************************************************************

import re, os

from gnatpython.fileutils import ls

from . cnotes import *
from . xnotep import *
from . tfiles import *
from . segments import *
from SUITE.control import LANGINFO, language_info
from SUITE.cutils import Identifier

# --------------------
# -- LnotesExpander --
# --------------------

# Construct a { source -> KnoteDict } dictionary of emitted Line Notes
# from =xcov outputs in files corresponding to a provided DOTXCOV_PATTERN
# (*.xcov for example).

class LnotesExpander:

    NK_for = {'.': lNoCode, '+': lFullCov, '-': lNoCov, '!': lPartCov,
              '#': lx0, '*': lx1}

    def process_tline(self, tline):
        m = re.match('\s*([0-9]+) (.):', tline.text)
        if m: self.elnotes[self.source].register (
            Enote (kind = self.NK_for[m.group(2)],
                   segment = Line(int(m.group(1))),
                   source = self.source))

    def listing_to_enotes(self, dotxcov):
        self.source = dotxcov.rsplit ('.', 1)[0]
        self.elnotes[self.source] = KnoteDict(elNoteKinds)
        Tfile (filename=dotxcov, process=self.process_tline)

    def __init__(self, dotxcov_pattern):

        # xcov --annotate=xcov produces a set of .xcov annotated unit sources,
        # each featuring a synthetic note per line.

        self.elnotes = {}
        [self.listing_to_enotes (dotxcov) for dotxcov in ls (dotxcov_pattern)]

# --------------------
# -- RnotesExpander --
# --------------------

# Construct a { source -> KnoteDict } dictionary of emitted Line Notes
# from a provided =report outputs.

# Report section classes, to let us control when looking for indication
# patterns and check that each appears in the section where we expect it.

# Abstract Report section

class Rsection:
    def __init__(self, name, re_start):
        self.name = name
        self.re_start = re_start

        self.n_starts = 0
        self.n_ends = 0

    def starts_on(self, rline):
        if not re.search (self.re_start, rline): return False

        self.n_starts += 1
        return True

    # def ends_on(self, rline):

    def check(self):
        thistest.fail_if (
            self.n_starts != self.n_ends,
            "(%s report section): %d starts != %d ends" % (
                self.name, self.n_starts, self.n_ends)
            )

    def value(self, count):
        return (0 if count=="No" else int(count))

# eNote sections (violations or exempted regions)

class Nsection (Rsection):

    def __init__(self, name, re_start):
        Rsection.__init__(self, name=name, re_start=re_start)

        self.enotes = []

    # def nkind_for(self, rline)

    def try_parse_enote(self, rline):

        # We expect something like:
        #
        #       "andthen.adb:10:33: statement not covered",
        #        -----------:-----: ---------------------
        #        source name:segmt: note text (-> note kind)

        # First, try to figure out the source name, one of our main dictionary
        # keys. Expect this to be a sequence of non-blank characters ending
        # with one of the possible source extensions we know about, preceding
        # a ':' character. Note that the registered extensions embed the '.'
        # character.

        xsrc = '([^ ]*)(%s):(.*)' % '|'.join (
            [ext for li in LANGINFO.values() for ext in li.src_ext])
        p = re.match(xsrc, rline)

        # If no match at all, punt.

        if not p: return None

        # Otherwise, proceed with the complete source name we found.

        source = ''.join ([p.group(1), p.group(2)])

        # Then, work over the trailing part, past the ':' character. Not
        # stricly necessary, but shorter so slightly more efficient.

        tail = p.group(3)

        nkind = self.nkind_for (tail)
        if nkind == None:
            thistest.failed (
                "(%s =report section) '%s' ?" % (
                    self.rs.name, rline.rstrip('\n')))
            return None

        segment = Section_within (tail)

        thistest.stop_if (
            not segment,
            FatalError ("Unable to parse report line\n'%s'" % rline))

        return Enote (kind=nkind, segment=segment, source=source)

    def try_parse(self, rline):
        enote = self.try_parse_enote(rline)
        if enote:
            self.enotes.append(enote)
        return enote

    def validate_ecount(self, count):
        self.ecount = len(self.enotes)
        thistest.fail_if (
            count != self.ecount,
            "(%s report section) recognized %d notes != summary (%d)\n" %
            (self.name, self.ecount, count))

        self.n_ends += 1
        return True

# Violations section

class Vsection (Nsection):
    def __init__(self, name, re_start, re_notes):
        Nsection.__init__(self, name=name, re_start=re_start)
        self.re_notes = re_notes

    def nkind_for(self, rline):
        for key in self.re_notes:
            if rline.find (key) != -1:
                return self.re_notes [key]
        return None

    def ends_on(self, rline):
        p = re.match ("(No|\d+) violation[s]*\.$", rline)
        return p and self.validate_ecount (count=self.value(p.group(1)))

# eXemptions section

class Xsection (Nsection):
    def __init__(self, name, re_start):
        Nsection.__init__(self, name=name, re_start=re_start)

    def nkind_for(self, rline):
        r = re.search (": (\d+) exempted violation", rline)
        return (None if not r
                else xBlock0 if int(r.group(1)) == 0 else xBlock1)

    def ends_on(self, rline):
        p = re.match ("(No|\d+) exempted region[s]*\.$", rline)
        return p and self.validate_ecount (count=self.value(p.group(1)))


# Analysis summary section

class Asection (Rsection):
    def __init__(self, name, re_start, skeys):
        Rsection.__init__(self, name=name, re_start=re_start)
        self.skeys = skeys
        self.checked = dict (
            [(sec, False) for sec in skeys])

    def try_match(self, sec, rline):
        p = re.match (self.skeys[sec], rline)

        if p:
            sum_count = self.value (p.group(1))
            sec_count = sec.ecount
            thistest.fail_if (
                sum_count != sec_count,
                "summary count %d != section count %d for %s" % (
                    sum_count, sec_count, sec.name)
                )
            thistest.fail_if (
                sec.n_starts != 1,
                "summary found for section starts != 1 (%s)" % sec.name
                )
            self.checked[sec] = True

    def try_parse(self, rline):
        [self.try_match(sec, rline)
         for sec in self.skeys if not self.checked[sec]]
        return None

    def ends_on(self, rline):
        p = re.match ("END OF REPORT$", rline)
        if p:
            self.n_ends += 1
        return p

    def check (self):
        Rsection.check (self)

        [thistest.fail_if (
                sec.n_starts > 0 and not self.checked[sec],
                "summary count check missing for section %s" % sec.name
                ) for sec in self.skeys
         ]

# Set of sections in a report

class RsectionSet:
    def __init__(self):
        self.Xr = Xsection (
            name="XR", re_start="EXEMPTED REGIONS"
            )

        self.Sc = Vsection (
            name="STMT", re_start="STMT COVERAGE",
            re_notes = {
                "statement not executed": sNoCov,
                "multiple statements on line": sPartCov}
            )

        self.Dc = Vsection (
            name="DECISION", re_start="DECISION COVERAGE",
            re_notes = {
                "decision outcome FALSE never": dfNoCov,
                "decision outcome TRUE never": dtNoCov,
                "decision never evaluated": dNoCov,
                "decision not exercised in both directions": dPartCov}
            )

        mcdc_notes =  {
            "decision outcome FALSE never": efNoCov,
            "decision outcome TRUE never": etNoCov,
            "decision never evaluated": eNoCov,
            "decision not exercised in both directions": ePartCov,
            "condition has no independent influence pair": cPartCov
            }

        self.Uc = Vsection (
            name="UC_MCDC", re_start="UC_MCDC COVERAGE",
            re_notes = mcdc_notes
            )

        self.Mc = Vsection (
            name="MCDC", re_start=" MCDC COVERAGE",
            re_notes = mcdc_notes
            )

        self.vsections = (self.Sc, self.Dc, self.Uc, self.Mc)

        self.nsections = self.vsections + (self.Xr,)

        self.As = Asection (
            name="AS", re_start="ANALYSIS SUMMARY",
            skeys = dict (
                [(s, "(No|\d+) non-exempted %s violation[s]*\.$" % s.name)
                 for s in self.vsections]
                + [(self.Xr, "(No|\d+) exempted region[s]*\.$")])
            )

        self.allsections = self.nsections + (self.As,)

    def starts_with (self, rline):
        for rs in self.allsections:
            if rs.starts_on (rline):
                return rs
        return None

    def check (self):
        [rs.check() for rs in self.allsections]

class RnotesExpander:
    """Produce list of Enote instances found in a "report" output."""

    def to_enotes(self, report):

        # We need to ignore everything not in the report sections
        # of interest, so until we know we're in ...

        self.rset = RsectionSet()
        self.rs = None

        self.report = report
        Tfile (filename=self.report, process=self.process_tline)

        self.rset.check()

    def register(self, enote):
        source = enote.source
        if source not in self.ernotes:
            self.ernotes[source] = KnoteDict(erNoteKinds)
        self.ernotes[source].register (enote)

    def process_tline(self, tline):

        rline = tline.text

        # Check if we are getting in a section of interest. If so, register
        # that and get to next line.

        rs = self.rset.starts_with (rline)
        if rs:
            self.rs = rs
            return None

        # Check if we are getting out of the current section of interest ...

        if self.rs and self.rs.ends_on(rline):
            self.rs = None

        # Skip this line if we're out of any section of interest

        if self.rs == None: return None

        enote = self.rs.try_parse(rline)

        # Some sections produce enotes, some don't (e.g. analysis summary).
        # An error is issued by the section processing if it should find one
        # but couldn't.

        if enote:
            self.register (enote)

        return enote

    def __init__(self, report):

        # xcov --annotate=report produces a single report featuring a list of
        # indications for slocs in all the units.

        self.ernotes = {}
        self.to_enotes (report)

# --------------------
# -- XnotesExpander --
# --------------------

# Construct two { source -> KnoteDict } dictionaries of expected coverage
# notes, one for line notes and one for report notes as expressed by user
# expectations found in a provided XFILE.

# We refer to the expressed user expectations as SCOV data, and parse it
# according to the following grammar:

#     SCOV.data := ucx_list
#     ucx_list := ucx <newline> [ucx_list]
#     ucx := sources <new_line> lx_list
#     sources := "--# " filename_list
#     filename_list := FILENAME [filename_list]
#     lx_list := lx <newline> [lx_list]
#     lx := "-- " lx_lre lx_lnote_list [lx_rnote_list] <newline>
#     lx_lre := "/" REGEXP "/"
#     weak_mark := ~
#     cov_level_choice := <s|d|m|u>
#     cov_level_list := cov_level_choice [cov_level_list]
#     cov_level_test := cov_level_list "=>"
#     lx_lnote_list := lx_lnote_choice [";" lx_lnote_list]
#     lx_lnote_choice := [cov_level_test] [weak_mark] lx_lnote
#     lx_lnote := <l-|l!|l+|l*|l#|l0>
#     lx_rnote_list := lx_rnote_choice [lx_rnote_list]
#     lx_rnote_choice := [cov_level_test] [weak_mark] lx_rnote
#     lx_rnote := <s-|s!|dT-|dF-|d!|eT-|eF-|oT-|oF-|c!|x0|x+>[:"TEXT"]

# The start of the SCOV data is identified as the first comment whose syntax
# matches a "sources" line.  Any comment before then is assumed to be a normal
# free-text comment.  Any comment thereafter is assumed to be part of the
# SCOV data.

# The "lx_lre" is a regular expression that is used to identify source lines
# that match "-- # " + lx_lre.  We decided to make the "-- # " implicit in
# order to reduce the lx_lre expression to a minimum as well as to force a
# standard format for all such markers.

# slashes inside lx_lre tokens are allowed. The SCOV_data parser simply
# uses the first and last slash as the delimiters.

# We use two intermediate abstractions to build the dictionaries from
# the expectations text:
#
# * Line Coverage eXpectations (LineCX) objects, to represent individual
#   expectations line like  "--  /bla/ l- s-", and
#
# * Unit Coverage eXpecations (UniCX) objects to represent the associations
#   of a sequence of line expectations with unit names.

# ------------
# -- LineCX --
# ------------

class LineCX:
    """Line Coverage eXpectations class.  Associated with source file names,
    these define unit coverage expectation specs."""

    def __init__(self, lre, lnp, rnps):
        self.lre = lre
        self.lnp = lnp
        self.rnps = rnps

    def instanciate_lnotes_over(self, tline, block, srules):
        return [self.lnp.instanciate_over (tline, block, srules)]

    def instanciate_rnotes_over(self, tline, block, srules):
        return [rnp.instanciate_over (tline, block, srules)
                for rnp in self.rnps if rnp.kind]

# ------------
# -- UnitCX --
# ------------

class UnitCX:
    """Associate a source name with a list of expected Coverage Line
    eXpectations. Construct Line and Report Xnote dictionaries."""

    def locate_source(self, source):
        """Helper for __init__. Return valid relative path were SOURCE may be
        found, searching plausible locations from the instantiation point."""

        for pdir in ["src/", "../src/"]:
            if os.path.exists(pdir+source):
                return pdir+source

        raise FatalError ("Unable to locate source %s" % source)

    # expected notes instanciations
    # -----------------------------

    def instanciate_notes_for(self, lx, tline, block, srules):
        [self.xldict.register (ln)
         for ln in lx.instanciate_lnotes_over (tline, block, srules)]
        [self.xrdict.register (rn)
         for rn in lx.instanciate_rnotes_over (tline, block, srules) if rn]

    # fuzz block processing
    # ---------------------

    # We identify block with the help of explicit comments, not with lone
    # language constructs such as begin/end in Ada. Finding the proper couples
    # of the latter is not easy and error prone.

    def blopen_p (self, tline):
        return re.match ("^\s*begin\s*-- #", tline.text)

    def blclose_p (self, tline):
        return re.match ("^\s*end;\s*-- #", tline.text)

    def check_block_on (self, tline):

        if self.blopen_p(tline):
            self.current_block = Block (parent = self.current_block)

        if self.blclose_p(tline):
            thistest.stop_if (
                not self.current_block,
                FatalError ("end of nonexistant block at\n=> " + tline.text))
            self.current_block = self.current_block.parent

    # kind subsitution rules processing
    # ---------------------------------

    subst_tuples_for = {
        "o/d": {otNoCov: dtNoCov,
                ofNoCov: dfNoCov,
                oNoCov : dNoCov},

        "o/e": {otNoCov: etNoCov,
                ofNoCov: efNoCov,
                oNoCov : eNoCov}
        }

    def check_srules_on (self, tline):

        # Check for kind substitution rules on this line. Reset
        # at every line for now.

        m = re.search ("# .*:(.*):$", tline.text)

        if not m:
            self.current_srules = None
        else:
            self.current_srules = {}
            [self.current_srules.update (self.subst_tuples_for [sim])
             for sim in m.group(1).split(',')]

    # toplevel processing
    # -------------------

    def process_tline(self, tline):
        self.check_srules_on (tline)
        [self.instanciate_notes_for (
                lx, tline, self.current_block, self.current_srules)
         for lx in self.LXset if re.search (lx.lre, tline.text)]
        self.check_block_on (tline)

    def __init__(self, source, LXset):

        self.LXset = LXset

        # dictionaries of expected line and report notes for our unit

        self.xldict = KnoteDict(xlNoteKinds)
        self.xrdict = KnoteDict(xrNoteKinds)

        self.source = source
        self.current_block = None
        self.current_srules = {}
        self.tfile  = Tfile (filename=self.locate_source(source),
                             process=self.process_tline)

        thistest.stop_if (
            self.current_block, FatalError ("fuzz block still open at EOF"))

# --------------------
# -- XnotesExpander --
# --------------------

# We need to parse things slightly differently for different languages. For
# example, expectation lines or expectation anchors which start with a comment
# marker.
#
# The comment marker for expectation lines depends on the language where the
# expectation lines are found, which migh be a test driver or a consolidation
# spec).
#
# The comment marker for expectation anchors depends on the language of the
# unit where this anchor will be matched.
#
# For example, a multi language expectations spec in an Ada driver or a
# consolidation spec would look like:
#
#   comment marker for an Ada test driver or a consolidation spec
#   v
#   --# foo.adb
#   --  /foo-ref/  ...  (search for "-- # foo-ref" in foo.adb)
#                                    ^ comment marker for Ada
#   --# bar.c
#   --  /bar-ref/  ...  (search for "// # bar-ref" in bar.c)
#                                    ^ comment marker for C
#
# In a C test driver, the leading comment marker would be "//".

class XnotesExpander:

    def __init__(self, xfile, xcov_level):
        self.xcov_level = xcov_level
        self.xlnotes = {}
        self.xrnotes = {}
        [self.to_xnotes(ux) for ux in
         self.__parse_scovdata (self.__get_scovdata (xfile))]

    def to_xnotes(self, ux):
        self.xlnotes [ux.source] = ux.xldict
        self.xrnotes [ux.source] = ux.xrdict

    def __get_scovdata(self, scov_file):
        """Return a list of strings containing the SCOV_data.
        To simplify parsing, the leading comment markers are stripped.
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
            if line.startswith(lang_info.comment + '#'):
                in_scovdata = True
            if in_scovdata and line.startswith(lang_info.comment):
                # Also take this opportunity to strip the leading comment
                # string as well as any space immediately following it.
                # This will simplify the parsing a little bit.
                contents.append(line[len(lang_info.comment):].lstrip())
        return contents

    # builtin markers support: to let test writers put things like
    #
    #    -- # __l-s-
    #
    # in the functional sources to mean: unless explicitly overriden by
    # a regular expectation spec, expect l- s- for this line always.

    # What default notes we expect for what designator text

    builtin_lxs = {"__l-s-": "l- s-",
                   "__l!d!": "l! d!"
                   }

    def __builtin_lcxs(self, ucx):
        """Builtin default Line coverage expectations, added to every
           Unit coverage expectations spec unless already there in UCX"""

        # Fetch the explicit line expectations from UCX and compute those
        # not there for which we have a default to provide. Beware that the
        # expressions in UCX were wrapped by parse_lcx already

        ux_lres = [lcx.lre for lcx in ucx[1]]
        nothere = [lre for lre in self.builtin_lxs if lre not in ux_lres]

        # Now compute the list of LCX objects for each of those defaults

        return [self.__parse_lcx("/%s/ %s" % (lre, self.builtin_lxs[lre]))
                for lre in nothere]

    def __wrap_lre(self, lx, langinfo):
        """For a source expressed in the language described by LANGINFO,
        adjust line regular expression in LX to expect it prefixed with
        "xx # " where "xx" is the language comment marker."""

        lx.lre = langinfo.comment + " # (" + lx.lre + ")"

        # The parens are crucial here. Consider what would happen for
        # /bla|blo/ without them ...

    def __register_ucx(self, ucx, uxset):
        """Add UCX to the set already in UXSET, adding builtin
           default expectations that were not overriden."""

        lxset = ucx[1]
        lxset.extend(self.__builtin_lcxs(ucx))

        # Wrap LREs to make sure we look for them in explicit anchors within
        # sources, not as arbitrary sections of source lines. Assume that all
        # the sources for this expectation block are in the same language.

        langinfo = language_info(ucx[0][0])
        [self.__wrap_lre(lx, langinfo) for lx in lxset]

        [uxset.append (UnitCX(source=source, LXset=lxset))
         for source in ucx[0]]

    def __parse_scovdata(self, scovdata):
        """Parse the given SCOV_DATA and return the corresponding
        list of UnitCX instances."""

        # The current UnitCX object being built.  We start a new UnitCX
        # everytime we see a "sources" line (which starts with '#', after
        # comment markers were stripped).
        current_ucx = (None, [])

        # The UXset being built while reading the scov data.
        UXset = []
        for line in scovdata:
            if line.startswith('#'):
                # We have finished reading the data for the current_ucx.
                # Build the associated UnitCX object, followed by resetting
                # current_ucx before we start reading the next ucx.
                if current_ucx[0] is not None:
                    self.__register_ucx(ucx=current_ucx, uxset=UXset)
                current_ucx = (self.__parse_sources(line), [])
            else:
                # This must be an LX line.
                current_ucx[1].append(self.__parse_lcx(line))

        if current_ucx[0] is not None:
            self.__register_ucx(ucx=current_ucx, uxset=UXset)

        return UXset

    def __parse_sources(self, image):
        """Given IMAGE as a string that contains a "sources" line,
        parse that line and return a list of source filenames."""

        # It's just a space-separated list of source files, with a leading
        # '#' character, so all we have to do is return a split of that
        # string, without the first '#'.
        return image.split()[1:]

    def __parse_one_expected_rnote(self, image):
        image=image.strip()
        if ':' in image:
            (noteim, stextim) = image.split(':')
            stext=stextim.strip('"')
        else:
            (noteim, stext) = (image, None)

        return XnoteP (text=self.__select_rnote(noteim), stext=stext)

    def __parse_expected_rnotes(self, image):
        if '#' in image:
            imlist = image.split('#')
        elif ',' in image:
            imlist = image.split(',')
        else:
            imlist = [image]
        return [self.__parse_one_expected_rnote(im) for im in imlist]

    def __parse_lcx(self, image):
        """Parse IMAGE as a string that contains a line expectation
        spec and return the corresponding LineCX object.
        """
        # Extract the LRE from the rest of the image.
        m = re.match("\s*/(.*)/\s+([^\s]*)( .*)?", image)
        if m is None:
            raise FatalError(
                "Invalid '%s' line expectation spec.\n" % image
                + "Expected /LRE/ lnote [rnotes]")

        lx_lre    = m.group(1)
        lx_lnote = XnoteP (text=self.__select_lnote (m.group(2)),
                           stext=None)

        thistest.stop_if (
            not m.group(3),
            FatalError ("Missing expected report notes in %s" % image))

        lx_rnotes = self.__parse_expected_rnotes(m.group(3))

        return LineCX(lx_lre, lx_lnote, lx_rnotes)

    def __decode_note_choice(self, text):
        """Given a note_choice that depends potentially on a list of coverage
        levels, return a list of (xcov-level, expected-note-text) tuples that
        represent those dependences.

        For instance, given:

            'u => l!' or 'sd => l+'

        ...this function will return:

            [('stmt+uc_mcdc', 'l!')]
            or
            [('stmt', 'l+'), ('stmt+decision', 'l+')]

        """
        level_from_char = {"s" : "stmt",
                           "d" : "stmt+decision",
                           "m" : "stmt+mcdc",
                           "u" : "stmt+uc_mcdc"}
        sep = "=>"
        result = text.split(sep)

        if len(result) == 1:
            # No choice
            return [("", text)]
        elif len(result) > 2:
            # Parse error
            raise FatalError("Note choice %s contains more than one arrow"
                             % text)
        else:
            note = result[1].lstrip(' ')
            lev_list = result[0].rstrip(' ')

            return [(level_from_char[lchar], note) for lchar in lev_list]

    def __select_lnote(self, text):
        """Decode text to return the line note for the current
        coverage level."""

        lx_lnote_list = text.split(";")

        level_table = dict(
            [ln_tuple for cond_notes in lx_lnote_list
             for ln_tuple in self.__decode_note_choice(cond_notes)])

        if level_table.has_key(self.xcov_level):
            return level_table [self.xcov_level]
        elif level_table.has_key(''):
            return level_table ['']
        else:
            raise FatalError(
                "Missing line expectation choice for level %s in %s"
                % (self.xcov_level, text))


    def __select_rnote(self, text):
        """Decode text to return the report note for the current
        coverage level."""
        level_table = dict(
            [ln_tuple for ln_tuple in self.__decode_note_choice(text)])

        if level_table.has_key(self.xcov_level):
            return level_table [self.xcov_level]
        elif level_table.has_key(''):
            return level_table ['']
        else:
            return "0"

