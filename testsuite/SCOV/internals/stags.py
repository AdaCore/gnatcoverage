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

from __future__ import annotations
from abc import abstractmethod
from dataclasses import dataclass, field
from typing import Self

import SUITE.control
from .segments import Sloc, Sloc_or_error


# Cache some values we need repeatedly

TARGET_INFO = SUITE.control.target_info()


@dataclass(frozen=True)
class Stag:
    """Abstract separation tag."""

    text: str

    def match(self, other: object) -> bool:
        return (
            isinstance(other, Stag)
            and self.__class__ == other.__class__
            and self.match_akin(other)
        )

    @abstractmethod
    def match_akin(self, other: Self) -> bool:
        pass


@dataclass(frozen=True)
class Itag(Stag):
    """
    Instance separation tag. Text is something like "sloc1[sloc2[sloc3]]" where
    each <sloc> is a <filename>:<slocsection> piece and where the brackets
    indicate instantiation nestings.
    """

    components: list[Sloc] = field(init=False, default_factory=list)

    def __post_init__(self) -> None:
        # use __setattr__ because frozen is set
        self.components.extend(
            [Sloc_or_error(part) for part in self.text.rstrip("]").split("[")],
        )

    def __all_components_match(self, other: Self) -> bool:
        # Check whether any component pair is found not to match:

        for c1, c2 in zip(self.components, other.components):
            if (
                c1.filename != c2.filename
                or c1.section.sp0.lineno != c2.section.sp0.lineno
            ):
                return False

        return True

    def match_akin(self, other: Self) -> bool:
        return len(self.components) == len(
            other.components
        ) and self.__all_components_match(other)


@dataclass(frozen=True)
class Rtag(Stag):
    """Routine separation tag. Text is routine name."""

    def match_akin(self, other: Self) -> bool:
        return self.text == other.text


def Stag_from(text: str, from_report: bool) -> Stag:
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
