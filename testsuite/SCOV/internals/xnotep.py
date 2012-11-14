# ***************************************************************************
# **                          EXPECTED NOTE PATTERNS                       **
# ***************************************************************************

# Expectation pattern, materializing the user level representation of a note
# expectation in drivers (text like "l+"). These get instanciated into a set
# of actual expected indications for precise segments when the line regular
# expressions are matched.

# This depends on the thittest instance.

# ***************************************************************************

from SUITE.context import thistest

from SUITE.cutils import lines_of, FatalError

from . cnotes import *
from . segments import *
from . tfiles import Tfile

# Different kind of expectations instanciate differently on a given source
# line. We introduce specialized note factories for this purpose:

# Block notes are relevant for a general section. Eventhough the block is
# matched line by line, we need to materialize a single note for the whole
# block.

class _XnoteP_block:

    def __init__(self, notep):
        self.notep  = notep
        self.lastni = None    # The last note instance we returned

    def instanciate_over(self, tline, block, kind):

        # We create a single instance the first time around, then expand the
        # section over subsequence matches.

        if self.lastni:
            thisni = None
            self.lastni.segment.sp1.l = tline.lno

        else:
            thisni = Xnote (xnp=self.notep, block=block, kind=kind)
            thisni.register_match (Section(
                    l0 = tline.lno, c0 = 0, l1 = tline.lno, c1 = 0))

        if thisni: self.lastni = thisni
        return thisni

# !block notes without a specific segment text are relevant to entire lines

class _XnoteP_line:

    def __init__(self, notep):
        self.notep = notep

    def instanciate_over(self, tline, block, kind):

        thisni = Xnote (xnp=self.notep, block=block, kind=kind)
        thisni.register_match (Line(tline.lno))

        return thisni

# !block notes with a specific segment subtext are relevant to that segment:
# we'll expect a reported note to designate a point within that subtext (most
# often, the beginning of it)

class _XnoteP_segment:

    def __init__(self, notep, stext):
        self.notep = notep
        self.stext = stext

    def instanciate_over(self, tline, block, kind):

        thisni = Xnote (xnp=self.notep, block=block, kind=kind)

        # Register matches for Segments corresponding to all the instances
        # of the subtext we find, then error out if too few or too many.

        [thisni.register_match (Segment (tline.lno, m.start()+1, m.end()))
         for m in re.finditer (self.stext, tline.text)]

        thistest.stop_if (
            thisni.nmatches == 0, FatalError (
                "couldn't find subtext '%s' in line '%s'"
                % (self.stext, tline.text)))

        thistest.stop_if (
            thisni.nmatches > 1, FatalError (
                "multiple matches of subtext '%s' in line '%s'"
                % (self.stext, tline.text)))

        return thisni

class XnoteP:

    NK_for = {'l.': lNoCode, 'l-': lNoCov, 'l!': lPartCov, 'l+': lFullCov,
              'l#': lx0, 'l*': lx1,
              's-': sNoCov, 's!': sPartCov,
              'dT-': dtNoCov, 'dF-': dfNoCov, 'd!': dPartCov, 'd-': dNoCov,
              'eT-': etNoCov, 'eF-': efNoCov, 'e!': ePartCov, 'e-': eNoCov,
              'oT-': otNoCov, 'oF-': ofNoCov, 'o!': oPartCov, 'o-': oNoCov,
              'c!': cPartCov,
              'x0': xBlock0, 'x+': xBlock1,
              '0': r0, '0c': r0c}

    def __init__(self, text, stext=None, stag=None):

        # WEAK conveys whether it is ok (not triggering test failure) for
        # expectations produced by this pattern not to be discharged by an
        # emitted note.

        self.weak = text[0] == '~'
        if self.weak: text = text[1:]

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

        thistest.stop_if (
            False and self.stext == None and self.kind in xNoteKinds,
            FatalError ("expected justification text required for %s" % text))

        # STAG is the separation tag that we must find on an emitted note to
        # discharge expectations produced from this pattern.

        self.stag = stag

        # Setup our instanciation factory now, which lets us perform the
        # required test only once:
 
        self.factory = (
            _XnoteP_block (notep=self) if block_p (self.kind)
            else 
            _XnoteP_line (notep=self) if not self.stext
            else 
            _XnoteP_segment (notep=self, stext=stext)
            )

    def instanciate_over (self, tline, block, srules):

        kind = (
            srules.__getitem__ (self.kind) if srules and self.kind in srules
            else self.kind
            )

        return self.factory.instanciate_over (
            tline=tline, block=block, kind=kind)

