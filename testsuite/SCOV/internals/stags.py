"""
SEPARATION TAGS

This module exposes abstractions over separation tags, indications that may
appear in =report diagnostics or =xcov+ extensions to complement violation
indications with machine code origin information.

We have two kinds of separation tags:

* those identifying code origins in terms of generic instantiation,
  triggered by the use of "gnatcov coverage -S instances"

* those identifying code origins in terms of object level routine name
  triggered by the use of "gnatcov coverage -S routines"
"""

import itertools

import SUITE.control
from .segments import Sloc_from


# Cache some values we need repeatedly

TARGET_INFO = SUITE.control.target_info()


class Stag:
    """Abstract separation tag."""
    def __init__(self, text):
        self.text = text

    def match(self, other):
        return (False
                if self.__class__ != other.__class__
                else self.match_akin(other))


def Stag_from(text, from_report):
    stag = Itag(text)
    if stag.components[0] is None:
        # Symbol names from report come from the binary file, so there is no
        # special processing for them.  Symbol names from expected notes
        # however need platform-specific transformations to match symbols from
        # binary files.
        if not from_report:
            text = TARGET_INFO.to_platform_specific_symbol(text)
        stag = Rtag(text)
    return stag


class Rtag(Stag):
    """Routine separation tag. Text is routine name."""
    def __init__(self, text):
        Stag.__init__(self, text)

    def match_akin(self, other):
        return self.text == other.text


class Itag(Stag):
    """
    Instance separation tag. Text is something like "sloc1[sloc2[sloc3]]" where
    each <sloc> is a <filename>:<slocsection> piece and where the brackets
    indicate instantiation nestings.
    """

    def __init__(self, text):
        Stag.__init__(self, text)
        self.components = [Sloc_from(part)
                           for part in text.rstrip(']').split('[')]

    def __all_components_match(self, other):

        # Check whether any component pair is found not to match:

        for c1, c2 in itertools.izip(self.components, other.components):
            if (
                c1.filename != c2.filename
                or c1.section.sp0.lineno != c2.section.sp0.lineno
            ):
                return False

        return True

    def match_akin(self, other):
        return (len(self.components) == len(other.components)
                and self.__all_components_match(other))
