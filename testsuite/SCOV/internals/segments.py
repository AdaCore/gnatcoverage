# ****************************************************************************
# **                             SOURCE SEGMENTS                            **
# ****************************************************************************

# This module exposes abstractions over source coordinates (line 4, line 5
# column 3, section from line 7 column 1 to line 9 column 8, ...), with
# facilities to check for inclusion of each within others.

# This is essentially used to determine if what an emitted coverage indication
# designates discharges some expected coverage expectation.

# ****************************************************************************

from SUITE.control import LANGINFO

import re

# ======================================
# == Section, Segment, Line and Point ==
# ======================================

# Spoint first, an internal helper which materializes a line:col coordinate
# and knows to determine if it is past or before another sloc for inclusion
# check purposes.

# The first logical column of a line is numbered 1. Column 0 is used in slocs
# designating a line as a whole. Any specific point on a line is considered to
# be within the line, so past-or-eq the beginning of it, or before-or-eq the
# end of it.

class Spoint:

    def __init__ (self, line, col):
        self.l = line
        self.c = col

    def pastoreq (self, other):
        return (self.l > other.l
                or (self.l == other.l
                    and (self.c >= other.c or other.c == 0)))

    def beforeq (self, other):
        return (self.l < other.l
                or (self.l == other.l
                    and (self.c <= other.c or other.c == 0)))

def Spoint_from(text):
    items = text.split (':', 1)
    return Spoint (
        line = int(items[0]), col = int(items [1]))

# Now the concrete classes. Each features a __str__ method for displays by the
# testuite (in error messages for example) and a "_from (text)" method to
# construct an instance from text known to be properly formed, coming from
# gnatcov outputs. The _str_ images do not need to match the format expected
# by their _from sibling in the same class.

# A Section is the association of two slocs to materialize the start
# and the end of a source section, and which knows to determine if it
# is included within another section.

class Section:
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

    def __init__ (self, l0, c0, l1, c1):
        self.sp0 = Spoint (line = l0, col = c0)
        self.sp1 = Spoint (line = l1, col = c1)

    def within(self, other):
        return (self.sp0.pastoreq (other.sp0)
                and self.sp1.beforeq (other.sp1))

    def __str__(self):
        return "section %d:%d-%d:%d" % (
            self.sp0.l, self.sp0.c, self.sp1.l, self.sp1.c)

def Section_from(text):
    topitems = text.split ('-', 1)
    subitems0 = topitems[0].split (':', 1)
    subitems1 = topitems[1].split (':', 1)
    return Section (
        l0 = int(subitems0[0]), c0 = int(subitems0[1]),
        l1 = int(subitems1[0]), c1 = int(subitems1[1]))

# A Segment is a Section for which the start and end are known to
# be on the same line.

class Segment (Section):
    def __init__ (self, lno, clo, chi):
        Section.__init__(self, l0 = lno, c0 = clo, l1 = lno, c1 = chi)

    def __str__(self):
        return "segment %d:%d-%d" % (self.sp0.l, self.sp0.c, self.sp1.c)

def Segment_from(text):
    topitems = text.split (':', 1)
    subitems = topitems[1].split ('-', 1)
    return Segment (
        lno = int(topitems[0]), clo = int(subitems[0]), chi = int(subitems[1]))

# A Line is a Segment spanning from first to last column

class Line (Segment):
    def __init__ (self, lno):
        Segment.__init__(self, lno = lno, clo = 0, chi = 0)

    def __str__(self):
        return "line %d" % self.sp0.l

def Line_from(text):
    items = text.split (':', 1)
    return Line (lno = int(items[0]))

# A Point is a Segment for which the start and end columns are identical.

class Point (Segment):
    def __init__ (self, lno, col):
        Segment.__init__(self, lno = lno, clo = col, chi = col)

    def __str__(self):
        return "sloc %d:%d" % (self.sp0.l, self.sp0.c)

def Point_from(text):
    items = text.split (':', 1)
    return Point (lno = int(items[0]), col = int(items[1]))

# Search and return a possible Section object with TEXT, specialized
# in accordance with the possible section expression shapes.

def Section_within(text):

    # Search for each possible shape in turn. Beware that the search order is
    # very relevant here.

    m = re.search ("(\d+:\d+-\d+:\d+)", text)
    if m: return Section_from (m.group(1))

    m = re.search ("(\d+:\d+-\d+)", text)
    if m: return Segment_from (m.group(1))

    m = re.search ("(\d+:\d+)", text)
    if m: return Point_from (m.group(1))

    m = re.search ("(\d+:)", text)
    if m: return Line_from (m.group(1))

    return None

# ==========
# == Sloc ==
# ==========

class Sloc:
    def __init__ (self, filename, section):
        self.filename = filename
        self.section = section

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

    re = '(?P<sbase>[^ ]*)(?P<ext>%s):(?P<sec>[^ ]*)' % '|'.join (
        [ext for li in LANGINFO.values() for ext in li.src_ext])

def Sloc_from_match (m):
    return Sloc (
        filename = ''.join ([m.group("sbase"), m.group("ext")]),
        section = Section_within (m.group("sec"))
        )

def Sloc_from (text):
    p = re.match (pattern=Sloc.re, string=text)
    return Sloc_from_match (m=p) if p else None
