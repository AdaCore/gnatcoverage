"""
SOURCE SEGMENTS

This module exposes abstractions over source coordinates (line 4, line 5 column
3, section from line 7 column 1 to line 9 column 8, ...), with facilities to
check for inclusion of each within others.

This is essentially used to determine if what an emitted coverage indication
designates discharges some expected coverage expectation.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import ClassVar, Self

import re

from SUITE.control import LANGINFO


@dataclass
class Spoint:
    """
    Internal helper which materializes a line:col coordinate and knows to
    determine if it is past or before another sloc for inclusion check
    purposes.

    The first logical column of a line is numbered 1. Column 0 is used in slocs
    designating a line as a whole. Any specific point on a line is considered
    to be within the line, so past-or-eq the beginning of it, or before-or-eq
    the end of it.
    """

    lineno: int
    c: int

    def pastoreq(self, other: Self) -> bool:
        return self.lineno > other.lineno or (
            self.lineno == other.lineno and (self.c >= other.c or other.c == 0)
        )

    def beforeq(self, other: Self) -> bool:
        return self.lineno < other.lineno or (
            self.lineno == other.lineno and (self.c <= other.c or other.c == 0)
        )


# Now the concrete classes. Each features a __str__ method for displays by the
# testuite (in error messages for example) and a "parse" method to construct an
# instance from text known to be properly formed, coming from gnatcov outputs.
# The __str__ images do not need to match the format expected by their "parse"
# sibling in the same class.


@dataclass
class Section:
    """
    A Section is the association of two slocs to materialize the start and the
    end of a source section, and which knows to determine if it is included
    within another section.
    """

    #        ...
    #  l0 -> 3: if a and then b then
    #           ^
    #          c0
    #        4:   val := klunk;
    #        5: else
    #        6:   val := junk;
    #  l1 -> 7: end if;
    #               ^
    #               c1
    #  3:1-7:5

    sp0: Spoint
    sp1: Spoint

    def within(self, other: Self) -> bool:
        return self.sp0.pastoreq(other.sp0) and self.sp1.beforeq(other.sp1)

    def __str__(self) -> str:
        return "section %d:%d-%d:%d" % (
            self.sp0.lineno,
            self.sp0.c,
            self.sp1.lineno,
            self.sp1.c,
        )

    @staticmethod
    def parse(text: str) -> Section:
        topitems = text.split("-", 1)
        subitems0 = topitems[0].split(":", 1)
        subitems1 = topitems[1].split(":", 1)
        return Section(
            sp0=Spoint(int(subitems0[0]), int(subitems0[1])),
            sp1=Spoint(int(subitems1[0]), int(subitems1[1])),
        )


class Segment(Section):
    """
    A Segment is a Section for which the start and end are known to be on the
    same line.
    """

    def __init__(self, lno: int, clo: int, chi: int):
        super().__init__(Spoint(lno, clo), Spoint(lno, chi))

    def __str__(self) -> str:
        return "segment %d:%d-%d" % (self.sp0.lineno, self.sp0.c, self.sp1.c)

    @staticmethod
    def parse(text: str) -> Segment:
        topitems = text.split(":", 1)
        subitems = topitems[1].split("-", 1)
        return Segment(
            lno=int(topitems[0]), clo=int(subitems[0]), chi=int(subitems[1])
        )


class Line(Segment):
    """A Line is a Segment spanning from first to last column."""

    def __init__(self, lno: int):
        super().__init__(lno=lno, clo=0, chi=0)

    def __str__(self) -> str:
        return "line %d" % self.sp0.lineno

    @staticmethod
    def parse(text: str) -> Line:
        items = text.split(":", 1)
        return Line(int(items[0]))


class Point(Segment):
    """
    A Point is a Segment for which the start and end columns are identical.
    """

    def __init__(self, lno: int, col: int):
        super().__init__(lno=lno, clo=col, chi=col)

    def __str__(self) -> str:
        return "sloc %d:%d" % (self.sp0.lineno, self.sp0.c)

    @staticmethod
    def parse(text: str) -> Point:
        items = text.split(":", 1)
        return Point(lno=int(items[0]), col=int(items[1]))


def Section_within(text: str) -> Section | None:
    """
    Search and return a possible Section object with TEXT, specialized in
    accordance with the possible section expression shapes.
    """

    # Search for each possible shape in turn. Beware that the search order is
    # very relevant here.

    m = re.search(r"(\d+:\d+-\d+:\d+)", text)
    if m:
        return Section.parse(m.group(1))

    m = re.search(r"(\d+:\d+-\d+)", text)
    if m:
        return Segment.parse(m.group(1))

    m = re.search(r"(\d+:\d+)", text)
    if m:
        return Point.parse(m.group(1))

    m = re.search(r"(\d+:)", text)
    if m:
        return Line.parse(m.group(1))

    return None


@dataclass(frozen=True)
class Sloc:
    filename: str
    section: Section

    # Regular expression to try-match a text line in order to
    # produce a valid object of this class

    # Expect something like:
    #
    #      "andthen.adb:10:33:
    #       -----------:-----:
    #       source name:segmt:
    #
    # Expect the source name to be a sequence of non-blank characters ending
    # with one of the possible source extensions we know about.

    # Note that the registered LANGINFO extensions embed the '.'  character,

    re: ClassVar[str] = (
        "(?P<sbase>[^ ]*)(?P<ext>%s):(?P<sec>[^ ]*)"
        % "|".join(ext for li in LANGINFO.values() for ext in li.src_ext)
    )


def Sloc_from_match(m: re.Match[str]) -> Sloc:
    section = Section_within(m.group("sec"))
    assert section is not None
    return Sloc(
        filename="".join([m.group("sbase"), m.group("ext")]),
        section=section,
    )


def Sloc_from(text: str) -> Sloc | None:
    p = re.match(pattern=Sloc.re, string=text)
    return Sloc_from_match(m=p) if p else None


def Sloc_or_error(text: str) -> Sloc:
    sloc = Sloc_from(text)
    assert sloc
    return sloc
