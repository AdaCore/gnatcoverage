"""
EMITTED LNOTE EXPANDERS

Expose the LnotesExpander class which constructs a { source -> KnoteDict }
dictionary of emitted Line Notes from =xcov outputs in files corresponding to a
provided DOTXCOV_PATTERN (*.xcov for example).
"""

import re

from e3.fs import ls

from .cnotes import (
    NK,
    KnoteDict,
    Enote,
    elNoteKinds,
)
from .segments import Line
from .tfiles import Tfile, Tline


class LnotesExpander:
    NK_for = {
        ".": NK.lNoCode,
        "0": NK.lNotCoverable,
        "?": NK.lUndetCov,
        "D": NK.lDisCov,
        "+": NK.lFullCov,
        "-": NK.lNoCov,
        "!": NK.lPartCov,
        "#": NK.lx0,
        "*": NK.lx1,
        "@": NK.lx2,
    }

    def process_tline(self, tline: Tline) -> None:
        m = re.match(r"\s*([0-9]+) (.):", tline.text)
        if m:
            self.elnotes[self.source].register(
                Enote(
                    kind=self.NK_for[m.group(2)],
                    segment=Line(int(m.group(1))),
                    source=self.source,
                )
            )

    def listing_to_enotes(self, dotxcov: str) -> None:
        self.source = dotxcov.rsplit(".", 1)[0]
        self.elnotes[self.source] = KnoteDict(elNoteKinds)
        Tfile(filename=dotxcov, process=self.process_tline)

    def __init__(self, dotxcov_pattern: str):
        # xcov --annotate=xcov produces a set of .xcov annotated unit sources,
        # each featuring a synthetic note per line.

        self.elnotes: dict[str, KnoteDict[Enote]] = {}
        for dotxcov in ls(dotxcov_pattern):
            self.listing_to_enotes(dotxcov)
