"""
EXPECTED NOTE PATTERNS

Expectation pattern, materializing the user level representation of a note
expectation in drivers (text like "l+"). These get instanciated into a set of
actual expected indications for precise segments when the line regular
expressions are matched.

This depends on the thittest instance.
"""

import re

from .cnotes import (
    Xnote, block_p, transparent_p, cPartCov, dNoCov, dPartCov, dfAlways,
    dfNoCov, dtAlways, dtNoCov, ePartCov, eNoCov, efNoCov, etNoCov, lFullCov,
    lNoCode, lNoCov, lNotCoverable, lPartCov, lx0, lx1, lx2, lx, oNoCov,
    oPartCov, ofNoCov, otNoCov, r0, r0c, sNoCov, sNotCoverable, sPartCov,
    xBlock0, xBlock1, xBlock2, xNoteKinds, lUndetCov, sUndetCov, dUndetCov,
    eUndetCov, XsNoCov, XsPartCov, XsNotCoverable, XsUndetCov, XotNoCov,
    XofNoCov, XoPartCov, XoNoCov, XcPartCov, Xr0, Xr0c, aNoCov, atNoCov,
    acPartCov
)
from .segments import Line, Section, Segment
from .stags import Stag_from
from SUITE.context import thistest
from SUITE.cutils import FatalError


# Different kind of expectations instanciate differently on a given source
# line. We introduce specialized note factories for this purpose:


class _XnoteP_block:
    """
    Block notes are relevant for a general section. Eventhough the block is
    matched line by line, we need to materialize a single note for the whole
    block.
    """

    def __init__(self, notep):
        self.notep = notep

        # The last note instance we returned
        self.lastni = None

    def instanciate_over(self, tline, block, kind):

        # We create a single instance the first time around, then expand the
        # section over subsequence matches.
        if self.lastni:
            thisni = None
            self.lastni.segment.sp1.lineno = tline.lno

        else:
            thisni = Xnote(xnp=self.notep, block=block, kind=kind)
            thisni.register_match(
                Section(l0=tline.lno, c0=0, l1=tline.lno, c1=0))

        if thisni:
            self.lastni = thisni
        return thisni


class _XnoteP_line:
    """
    !block notes without a specific segment text are relevant to entire lines.
    """

    def __init__(self, notep):
        self.notep = notep

    def instanciate_over(self, tline, block, kind):

        thisni = Xnote(xnp=self.notep, block=block, kind=kind)
        thisni.register_match(Line(tline.lno))

        return thisni


class _XnoteP_segment:
    """
    !block notes with a specific segment subtext are relevant to that segment:
    we'll expect a reported note to designate a point within that subtext (most
    often, the beginning of it).
    """

    def __init__(self, notep, stext):
        self.notep = notep
        self.stext = stext

    # For operands like "(bla > 3)" or "(bla(x, y) < 12)", a specified segment
    # subtext of the form "(bla*)" designates the entire operand, outer parens
    # included.

    def __extended_segend_for(self, tline, bm):
        """
        Compute the extended segment end for a provided BM (base match) object,
        xmatching the base specified subtext in TLINE, opening paren included
        and xup to the '*' character.
        """

        # Start with the end of the base match. Operate in string index units
        # first:

        segend = bm.end() - 1

        # Fetch characters up to the first closing paren that is not
        # closing an inner one. Guard against the case where this paren is
        # not on the line.

        linend = len(tline.text) - 1
        pnest = 0

        while segend <= linend and pnest >= 0:
            c = tline.text[segend]
            if c == '(':
                pnest += 1
            elif c == ')':
                pnest -= 1
            segend += 1

        # Unless we have hit the end of line early, the loop above has moved
        # segend one past the closing paren. This is exactly what we need to
        # return in sloc units, starting at 1 unlike array indexes.

        return segend

    def instanciate_over(self, tline, block, kind):

        thisni = Xnote(xnp=self.notep, block=block, kind=kind)

        # Register matches for Segments corresponding to all the instances of
        # the subtext we find, possibly extended.  Error out if too few or too
        # many.

        # Compute a base subtext to start from and whether we should extend or
        # not.

        base = self.stext
        if self.stext.startswith('(') and self.stext.endswith('*)'):
            base = self.stext[0:-2]
            extend = True
        else:
            extend = False

        # As we use RE services to find matches for the string, and their
        # position in the line, we need to escape it to make sure any special
        # character is not considered as part of a regexp expression.
        base = re.escape(base)
        for bm in re.finditer(pattern=base, string=tline.text):
            thisni.register_match(
                Segment(
                    tline.lno, bm.start() + 1,
                    self.__extended_segend_for(bm=bm, tline=tline)
                    if extend else bm.end()))

        thistest.stop_if(
            thisni.nmatches == 0,
            FatalError("couldn't find subtext '%s' in line '%s'"
                       % (self.stext, tline.text)))

        thistest.stop_if(
            thisni.nmatches > 1,
            FatalError("multiple matches of subtext '%s' in line '%s'"
                       % (self.stext, tline.text)))

        return thisni


class XnoteP:

    NK_for = {'l-': lNoCov, 'l!': lPartCov, 'l+': lFullCov,
              'l.': lNoCode, 'l0': lNotCoverable, 'l?': lUndetCov,
              'l#': lx0, 'l*': lx1, 'l@': lx2, 'l=': lx,
              's-': sNoCov, 's!': sPartCov, 's0': sNotCoverable,
              's?': sUndetCov,
              'dT*': dtAlways, 'dF*': dfAlways,
              'dT-': dtNoCov, 'dF-': dfNoCov, 'd!': dPartCov, 'd-': dNoCov,
              'd?': dUndetCov,
              'eT-': etNoCov, 'eF-': efNoCov, 'e!': ePartCov, 'e-': eNoCov,
              'e?': eUndetCov,
              'oT-': otNoCov, 'oF-': ofNoCov, 'o!': oPartCov, 'o-': oNoCov,
              'c!': cPartCov,
              'a-': aNoCov, 'aT-': atNoCov, 'ac!': acPartCov,
              'x0': xBlock0, 'x+': xBlock1, 'x?': xBlock2,
              '0': r0, '0c': r0c,
              # Exempted notes
              'Xs-': XsNoCov, 'Xs!': XsPartCov, 'Xs0': XsNotCoverable,
              'Xs?': XsUndetCov,
              'XoT-': XotNoCov, 'XoF-': XofNoCov, 'Xo!': XoPartCov,
              'Xo-': XoNoCov, 'Xc!': XcPartCov,
              'X0': Xr0, 'X0c': Xr0c}

    # The notes prefixed with 'X' correspond to the type of violations we
    # expect to encounter within an exempted block. In this context we cannot
    # determine if a violation on a decision belongs to decision or MCDC
    # coverage. Therefore we use the 'o' notes to express this lack of
    # information.

    def __init__(self, text, stext=None, stag=None):

        # WEAK conveys whether it is ok (not triggering test failure) for
        # expectations produced by this pattern not to be discharged by an
        # emitted note.

        self.weak = text[0] == '~'
        if self.weak:
            text = text[1:]

        # KIND is the kind of note this expectation stands for

        self.kind = self.NK_for[text]

        # STEXT is a substring in matching source lines where the sloc of an
        # emitted note must fall to discharge. For example:
        #
        #         6 - 10
        #         v   v
        #  4:  if Cond1 and then Cond2 then  -- # expr
        #
        #  /expr/  c!:"Cond1"
        #
        # yields "Cond1" in stext meaning that we must find an emitted note
        # with a sloc pointing somewhere within col 6 and col 10 to discharge
        # the expectation for line 4 here.

        self.stext = stext

        # We could require and use stext to store expected justification text
        # for exemptions. We don't handle that as of today.

        thistest.stop_if(
            False and self.stext is None and self.kind in xNoteKinds,
            FatalError("expected justification text required for %s" % text))

        # STAG is the separation tag that we must find on an emitted note to
        # discharge expectations produced from this pattern. Initially, at this
        # __init__ point, this is set with the stag text found.

        self.stag = stag

        # Setup our instanciation factory now, which lets us perform the
        # required test only once:

        self.factory = (
            _XnoteP_block(notep=self)
            if block_p(self.kind) else
            _XnoteP_line(notep=self)
            if not self.stext else
            _XnoteP_segment(notep=self, stext=stext))

    def instantiate_stag(self):
        self.stag = Stag_from(self.stag, False)

    def instanciate_over(self, tline, block, srules):

        kind = (srules[self.kind]
                if srules and self.kind in srules
                else self.kind)

        return (None
                if kind is None or transparent_p(kind)
                else self.factory.instanciate_over(tline=tline, block=block,
                                                   kind=kind))
