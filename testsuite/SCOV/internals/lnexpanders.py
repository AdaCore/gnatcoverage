# ****************************************************************************
# **                         EMITTED LNOTE EXPANDERS                        **
# ****************************************************************************

# Expose the LnotesExpander class which constructs a { source -> KnoteDict }
# dictionary of emitted Line Notes from =xcov outputs in files corresponding
# to a provided DOTXCOV_PATTERN (*.xcov for example).

# ****************************************************************************

import re

from gnatpython.fileutils import ls

from . cnotes import KnoteDict, Enote, elNoteKinds
from . cnotes import lNoCode, lFullCov, lPartCov, \
                     lNoCov, lNotCoverable, lx0, lx1
from . segments import Line
from . tfiles import Tfile

# ====================
# == LnotesExpander ==
# ====================

class LnotesExpander:

    NK_for = {'.': lNoCode, '0': lNotCoverable,
              '+': lFullCov, '-': lNoCov, '!': lPartCov,
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
