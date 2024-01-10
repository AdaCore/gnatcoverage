############################################################################
#                                                                          #
#                       The Qualifying Machine (QM)                        #
#                                                                          #
#                     Copyright (C) 2010-2014, AdaCore                     #
#                                                                          #
# The QM is free software; you can redistribute it  and/or modify it       #
# under terms of the GNU General Public License as published by the Free   #
# Software Foundation; either version 3, or (at your option) any later     #
# version.  The QM is distributed in the hope that it will be useful,      #
# but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  #
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public #
# License  for more details. You  should  have  received a copy of the GNU #
# General Public License  distributed with the QM; see file COPYING3. If   #
# not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite #
# 330, Boston, MA 02111-1307, USA.                                         #
#                                                                          #
############################################################################

"""This module handles the ReST related tasks. This involves both docutils used
   for live previews and sphinx for the final documents creation"""
from __future__ import annotations

import os.path
import hashlib

from typing import Optional, Sequence

ARTIFACT_IMPORTER_PLUGINS = {}

"""Implement some utility function that ease generation of ReST code"""

HEADING_CHARS = ["#", "*", "=", "-", "^", '"']


def heading(title, heading_char):
    if isinstance(heading_char, int):
        heading_char = HEADING_CHARS[heading_char]

    result = "\n\n"
    result += heading_char * len(title) + "\n"
    result += title + "\n"
    result += heading_char * len(title) + "\n\n"
    return result


def part(title):
    return heading(title, "#")


def chapter(title):
    return heading(title, "*")


def section(title):
    return heading(title, "=")


def subsection(title):
    return heading(title, "-")


def subsubseciton(title):
    return heading(title, "^")


def paragraph(title):
    return heading(title, '"')


def toctree(itemlist, depth=2, attrlist=()):
    return (
        "\n".join(
            [".. toctree::", "   :maxdepth: %s" % depth]
            + ["   %s" % attr for attr in attrlist]
            + ["\n"]
            + ["   %s" % item for item in itemlist]
        )
        + "\n"
    )


def emphasis(content):
    return "*" + content + "*"


def strong(content):
    return "**" + content + "**"


def generic_block(command, content, command_arg=None):
    if content == "":
        return ""

    if command_arg is None:
        command_arg = ""

    result = "\n.. %s:: %s\n\n" % (command, command_arg)
    result += "\n".join(["   " + line for line in content.splitlines()])
    result += "\n\n"
    return result


def warning(content):
    return generic_block("warning", content)


def parsed_literal(content):
    return generic_block("parsed-literal", content)


def code_block(content, language):
    return generic_block("code-block", content, language)


def raw(content, doc_type):
    return generic_block("raw", content, doc_type)


def line_block(content):
    result = "\n\n"
    result += "\n".join(["   " + line for line in content.splitlines()])
    return result


def list(content):  # noqa: A001
    result = "\n\n"
    result += "\n".join(["    * %s" % line for line in content])
    result += "\n\n"
    return result


class Artifact(object):
    pass

    def get_children(self: Artifact) -> Sequence[Artifact]:
        """Return list of subartifacts"""
        return []

    def get_child(self: Artifact, name: str) -> Artifact | None:
        """Return child artifact by name"""
        stripped_name = name.strip()
        for a in self.get_children():
            if a.get_name() == stripped_name:
                return a

        return None

    def get_name(self: Artifact) -> str:
        return ""

    @property
    def full_name(self: Artifact) -> str:
        return ""

    def dtext(self: Artifact) -> str:
        """Return text of the artifact"""
        return ""

    def title(self: Artifact) -> str:
        """Return first line of artifact dtext(), remove formatting"""
        content = self.dtext()
        for line in content.splitlines():
            line = line.strip().strip("*")
            if len(line) > 0 and not line.startswith(".. "):
                #  ** has to be removed
                #  from short_description when used in tables
                line = line.replace("**", "")
                import re

                # remove all directives
                line = re.sub(r":[a-zA-Z0-9]*:", "", line)
                # replace backquotes with single quotes
                line = line.replace("`", "'")

                return line

        return ""

    def docfile(self: Artifact, no_extension: bool = False) -> str:
        """Filename for the generated RST file"""
        return ""

    def rest_doc_ref(self: Artifact) -> str:
        """Returns a sphinx :doc: reference"""
        return ""

    @property
    def relatives(self: Artifact) -> Sequence[Artifact]:
        """
        The list of sub-artifacts that are relative to self.

        :rtype: A list of :class:`Artifact`
        """
        return []

    @property
    def relative_to(self: Artifact) -> Optional[Artifact]:
        """
        The instance this artifact is relative to.

        :rtype: An instance of :class:`Artifact`, or None
        """
        return None

    def hash(self: Artifact) -> str:  # noqa: A003
        """Return a hash based on the artifact full name"""
        hashobj = hashlib.new("sha1")
        hashobj.update(self.full_name.encode("utf-8"))
        return hashobj.hexdigest()


class MetaArtifactImporter(type):
    """
    Metaclass for Artifact Importers: automatically registers new
    importers.
    """

    def __new__(mcs, classname, bases, class_dict):
        result = type.__new__(mcs, classname, bases, class_dict)
        if classname != "ArtifactImporter":
            ARTIFACT_IMPORTER_PLUGINS[classname] = result
        return result


class ArtifactImporter(metaclass=MetaArtifactImporter):
    """
    ArtifactImporter is responsible for the translation of an artifact to
    rest format.
    To declare a new Importer, just override the
    :func:`ArtifactImporter.to_rest`.
    """

    QMLINK_TAG = ".. qmlink::"

    def __init__(self):
        self._concatenate = False

    @classmethod
    def new(cls, name):
        """
        Given an importer class name, return a new instance of that importer

        :param str name: The name of the class
        :return: an ArtifactImporter instance
        """

        if name not in ARTIFACT_IMPORTER_PLUGINS:
            raise Exception("No such importer: %s" % name)
        return ARTIFACT_IMPORTER_PLUGINS[name]()

    @staticmethod
    def is_qmlink(line: str) -> bool:
        return line.startswith(ArtifactImporter.QMLINK_TAG)

    @property
    def concatenate(self):
        """
        Wether the imported artifacts should be directly concatenated (if
        several are present in the same qmlink directive) or should have
        separate content.

        :return: A boolean (False by default)
        """
        return self._concatenate

    @concatenate.setter
    def concatenate(self, value):
        "concatenate setter"
        self._concatenate = value

    def copy_resources(self, artifact, dest_dir):
        """
        Copies additional resources to the destination directory if needed, for
        documentation generation purpose.

        By default, this is a no-op.

        :param artifact: The artifact being imported
        :type artifact: a :class:`qm.Artifact`
        :param dest_dir: The destination directory
        :type dest_dir: A string
        """
        pass

    def to_rest(self, artifact):
        """
        Needs to be overriden.

        :param Artifact: the artifact to translate
        :type Artifact: an instance of :class:`qm.Artifact`

        :rtype: a string
        """

    def qmlink_to_rest(self, parent, artifacts):
        """
        Needs to be overriden.
        """
        raise NotImplementedError("not implemented")


class DefaultImporter(ArtifactImporter):
    """
    The default importer, used for importing artifacts with files already
    in rest format
    """

    def to_rest(self, artifact):
        return artifact.dtext()
