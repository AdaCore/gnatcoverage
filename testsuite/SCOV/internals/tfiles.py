"""
TEXT LINES AND FILES

This module exposes the Tline and Tfile classes to help managing text files and
their contents.

Essentially, Tline allows the association of a line number with each line and
Tfile offers a line hook processing facility.
"""

from dataclasses import dataclass
from typing import Callable

from .cnotes import Enote


@dataclass(frozen=True)
class Tline:
    """Associate a line contents with its position in a text file."""

    lno: int
    text: str


class Tfile:
    """
    Abstract a set of Tlines from a provided filename, each PROCESSed as read
    at class instanciation time.
    """

    def __init__(
        self, filename: str, process: Callable[[Tline], Enote | None]
    ) -> None:
        self.nlines: int = 0
        self.process = process
        self.tlines = [self.new_tline(text) for text in open(filename)]

    def new_tline(self, text: str) -> Tline:
        self.nlines += 1
        tline = Tline(self.nlines, text)
        self.process(tline)
        return tline

    def contents(self) -> list[Tline]:
        return self.tlines
