# ****************************************************************************
# **                              SEPARATION TAGS                           **
# ****************************************************************************

# This module exposes abstractions over separation tags, indications
# that may appear in =report diagnostics or =xcov+ extensions to complement
# violation indications with machine code origin information.
#
# We have two kinds of separation tags:
#
# * those identifying code origins in terms of generic instantiation,
#   triggered by the use of "gnatcov coverage -S instances"
#
# * those identifying code origins in terms of object level routine name
#   triggered by the use of "gnatcov coverage -S routines"

# ****************************************************************************

from . segments import Sloc, Sloc_from

# ==========
# == Stag ==
# ==========

# Abstract separation tag

class Stag:
    def __init__ (self, text):
        self.text = text

    def match (self, other):
        return (
            False if self.__class__ != other.__class__
            else self.match_akin (other)
            )

def Stag_from (text):
    stag = Itag (text)
    if stag.components[0] is None:
        stag = Rtag (text)
    return stag

# ==========
# == Rtag ==
# ==========

# Routine separation tag. Text is routine name.

class Rtag (Stag):
    def __init__ (self, text):
        Stag.__init__ (self, text)

    def match_akin (self, other):
        return self.text == other.text

# ==========
# == Itag ==
# ==========

# Instance separation tag. Text is something like "sloc1[sloc2[sloc3]]" where
# each <sloc> is a <filename>:<slocsection> piece and where the brackets
# indicate instantiation nestings.

class Itag (Stag):

    def __init__ (self, text):
        Stag.__init__ (self, text)
        self.components = [
            Sloc_from (part) for part in text.rstrip(']').split('[')
            ]

    def __all_components_match (self, other):
        l1 = self.components
        l2 = other.components

        while l1:
            p1 = l1.pop ()
            p2 = l2.pop ()

            if (p1.filename != p2.filename
                or
                p1.section.sp0.l != p2.section.sp0.l
                ):
                return False

        return True

    def match_akin (self, other):
        return (
            len (self.components) == len (other.components)
            and self.__all_components_match (other)
            )

