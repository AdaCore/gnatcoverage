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

# ------------
# -- fb_get --
# ------------

def fb_get(dict, key):
    """Get DICT[KEY], falling back to DICT[''] if KEY is not a
    a valid key in DICT"""
    return dict.get(key, dict[""])

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
                   segment = Line(int(m.group(1)))))

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
# from =xcov outputs in files corresponding to a provided DOTXCOV_PATTERN

class RnotesExpander:
    """Produce list of Enote instances found in a "report" output."""

    NK_for = {"decision outcome FALSE never": dfNoCov,
              "decision outcome TRUE never": dtNoCov,
              "decision never evaluated": dNoCov,
              "decision not exercised in both directions": dPartCov,
              "multiple statement SCOs": sPartCov,
              "condition has no independent influence pair": cPartCov,
              "statement not executed": sNoCov}

    def nkind_for(self, ntext):

        # Search for a possible straight correspondance first

        for key in self.NK_for:
            if ntext.find (key) != -1:
                return self.NK_for [key]

        # If we haven't found a match, check for exemption notes. The text
        # will tell us if we have one, and we need to look at the number of
        # exempted violations to finalize.

        r = re.search (": (\d+) exempted violation", ntext)
        if r: return xBlock0 if int(r.group(1)) == 0 else xBlock1

        return None

    def to_enotes(self, report):

        # We need to ignore everything not in the report sections
        # of interest, so until we know we're in ...
        self.section = rsNoInterest

        self.report = report
        Tfile (filename=self.report, process=self.process_tline)

    def register(self, source, enote):
        if source not in self.ernotes:
            self.ernotes[source] = KnoteDict(erNoteKinds)
        self.ernotes[source].register (enote)

    def process_tline(self, tline):

        rline = tline.text

        # Figure out which section we're [getting] in.  Beware that
        # the ordering of the regexp checks matters here.

        if re.search ("NON-EXEMPTED VIOLATIONS", rline):
            self.section = rsNotExempted
            return None
        elif re.search ("EXEMPTED REGIONS", rline):
            self.section = rsExempted
            return None
        elif re.match (".* (violation|region)\.$", rline):
            # Getting out of a section of interest ...
            self.section = rsNoInterest

        if self.section == rsNoInterest: return None

        # In section of interest, match the emitted note text. We expect
        # something like "andthen.adb:10:33: statement not covered",
        #                 -----------:-----: ---------------------
        #                 source name:segmt: note text (-> note kind)

        # First, try to figure out the source name, one of our main dictionary
        # keys. If no match at all, skip.

        xsrc = '([^ ]*)\.(ads|adb):(.*)'
        p = re.match(xsrc, rline)
        if not p:
            return None

        source = "%s.%s" % (p.group(1), p.group(2))

        # Then, work over the trailing part. Not stricly necessary, but
        # shorter so slightly more efficient.

        tail = p.group(3)

        nkind = self.nkind_for (tail)
        if nkind == None:
            thistest.failed (
                "(%s) '%s' ?" % (self.report, rline.rstrip('\n')))
            return None

        section = Section_within (tail)

        thistest.stop_if (
            not section,
            FatalError ("Unable to parse report line\n'%s'" % rline))

        return self.register (
            source, Enote (kind=nkind, segment=section, rsid=self.section))

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
#     cov_level_test := <s|d|m|u> "=>"
#     lx_lnote_list := lx_lnote_choice [";" lx_lnote_list]
#     lx_lnote_choice := [cov_level_test] [weak_mark] lx_lnote
#     lx_lnote := <l-|l!|l+|l*|l#|l0>
#     lx_rnote_list := lx_rnote_choice [lx_rnote_list]
#     lx_rnote_choice := [cov_level_test] [weak_mark] lx_rnote
#     lx_rnote := <s-|s!|dT-|dF-|d!|u!|m!|x0|x+>[:"TEXT"]

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

    def instanciate_lnotes_over(self, tline, block):
        return [self.lnp.instanciate_over (tline, block)]

    def instanciate_rnotes_over(self, tline, block):
        return [rnp.instanciate_over (tline, block)
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

    def instanciate_notes_for(self, lx, tline, block):
        [self.xldict.register (ln)
         for ln in lx.instanciate_lnotes_over (tline, block)]
        [self.xrdict.register (rn)
         for rn in lx.instanciate_rnotes_over (tline, block) if rn]

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

    # toplevel processing
    # -------------------

    def process_tline(self, tline):
        [self.instanciate_notes_for (lx, tline, self.current_block)
         for lx in self.LXset if re.search (lx.lre, tline.text)]
        self.check_block_on (tline)

    def __init__(self, source, LXset):
        self.LXset = LXset

        # dictionaries of expected line and report notes for our unit

        self.xldict = KnoteDict(xlNoteKinds)
        self.xrdict = KnoteDict(xrNoteKinds)

        self.source = source
        self.current_block = None
        self.tfile  = Tfile (filename=self.locate_source(source),
                             process=self.process_tline)

        thistest.stop_if (
            self.current_block, FatalError ("fuzz block still open at EOF"))

# --------------------
# -- XnotesExpander --
# --------------------

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
        To simplify parsing, the leading "--" is also stripped.
        """

        # The scov data begins at the first line that starts with
        # a '--#' comment.  Any line that starts as a comment after
        # this first '--#' line is assumed to be part of the scov data.
        # Build a list of lines containing the scov data stored in
        # scov_file now.
        contents = []
        in_scovdata = False
        for line in lines_of(scov_file):
            # Take care of leading/trailing spaces to give the user
            # more flexibility.  Also take care of the trailing new-line
            # character that we get from lines_of.
            line.strip()
            if line.startswith('--#'):
                in_scovdata = True
            if in_scovdata and line.startswith('--'):
                # Also take this opportunity to strip the leading '--'
                # as well as any space immediately followint it.  This
                # will simplify the parsing a little bit.
                contents.append(line[2:].lstrip())
        return contents

    def __wrap_lre(self, lre):
        """The actual expression we match against source lines for
           a "/LRE/" expressed expectation"""

        # The parens are crucial here. Consider what would happen for
        # /bla|blo/ without them ...

        return "-- # (" + lre + ")"

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
        nothere = [lre for lre in self.builtin_lxs
                   if self.__wrap_lre(lre) not in ux_lres]

        # Now compute the list of LCX objects for each of those defaults

        return [self.__parse_lcx("/%s/ %s" % (lre, self.builtin_lxs[lre]))
                for lre in nothere]

    def __register_ucx(self, ucx, uxset):
        """Add UCX to the set already in UXSET, adding builtin
           default expectations that were not overriden."""

        ucx[1].extend(self.__builtin_lcxs(ucx))

        [uxset.append (UnitCX(source=source, LXset=ucx[1]))
         for source in ucx[0]]

    def __parse_scovdata(self, scovdata):
        """Parse the given SCOV_DATA and return the corresponding
        list of UnitCX instances."""

        # The current UnitCX object being built.  We start a new UnitCX
        # everytime we see a "sources" line (which starts with '--#').
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

        return LineCX(self.__wrap_lre(lx_lre), lx_lnote, lx_rnotes)

    def __decode_note_choice(self, text):
        """Given a note_choice that depends potentially on the coverage
        level, return a list that represents this dependance,
        whose first element is the coverage level that it depends on
        (or an empty string if the note does not depend on the level)
        and whose second element is the expectation note if the choice
        is taken.

        For instance, given:

            'u => l!'

        ...this function will return:

            ['stmt+uc_mcdc', 'l!']

        """
        level_from_char = {"s" : "stmt",
                           "d" : "stmt+decision",
                           "m" : "stmt+mcdc",
                           "u" : "stmt+uc_mcdc"}
        sep = "=>"
        result = text.split(sep)

        if len(result) == 1:
            # No choice
            return ["", text]
        elif len(result) > 2:
            # Parse error
            raise FatalError("Note choice %s contains more than one arrow"
                             % text)
        else:
            return [level_from_char[result[0]], result[1]]

    def __select_lnote(self, text):
        """Decode text to return the line note for the current
        coverage level."""
        lx_lnote_list = text.split(";")
        level_table = dict([self.__decode_note_choice(conditional_note)
                            for conditional_note in lx_lnote_list])

        if not level_table.has_key(''):
            raise FatalError("No default case in line expectation: %s" % text)

        return fb_get(level_table, self.xcov_level)

    def __select_rnote(self, text):
        """Decode text to return the report note for the current
        coverage level."""
        level_table = dict([['', "0"], self.__decode_note_choice(text)])
        return fb_get(level_table, self.xcov_level)
