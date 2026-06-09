"""
TEXT LINES AND FILES

This module exposes the Tline and Tfile classes to help managing text files and
their contents.

Essentially, Tline allows the association of a line number with each line and
Tfile is just a list of Tline.
"""

from dataclasses import dataclass
from typing import Iterator


@dataclass(frozen=True)
class Tline:
    """Associate a line contents with its position in a text file."""

    lno: int
    text: str


class Tfile:
    """Abstract a set of Tlines from a provided filename."""

    def __init__(self, filename: str) -> None:
        with open(filename) as f:
            self.tlines = [Tline(lno, text) for lno, text in enumerate(f, 1)]

    def __iter__(self) -> Iterator[Tline]:
        return iter(self.tlines)
