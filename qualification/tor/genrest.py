#!/usr/bin/env python
from __future__ import annotations

import os
import glob
import re
import sys
import json
import optparse

from tor.genrest_conf import ALLOW_UNCLASSIFIED_FOLDERS, SUPRESS_MSG_TYPES

import scripts.rest as rest
import scripts.logger as logger
from scripts.rest import Artifact, ArtifactImporter, writer
from scripts.qm_plugins.importers import (
    resolve_importer,
    default_importer,
)
from itertools import zip_longest
from scripts.common_conf import tor_doc_id
from tor.proj_conf import get_var_replacements

from typing import Callable, Dict, List, Set, Optional, Sequence, Any, IO

# *****************************************************************************
# **                            GENERAL DESCRIPTION                          **
# *****************************************************************************

# This file implements the core of the GNATcoverage TOR document production,
# generating sphinx-REST from artifacts held within our testsuite/Qualif
# filesystem tree.

# Except on rare occasions, each directory in this tree represents some TOR
# related entity: requirement, testcase, appendix material, or groups of such,
# possibly nested (groups may hold direct artifacts or further intermediate
# groups).
#
# The general structure is very hierarchical, going from requirement
# groups (REQG kind) to requirements (REQ kind), to testcases (TC kind)
# possibly via testcase groups (TCG kind).
#
# Very roughly, our processing here first constructs an internal tree
# datastructure representing the directory tree, where each node holds
# attributes qualifying its nature (e.g. "this node/directory holds a
# testcase"). This is determined from two elements:
#
# * The presence in the directory of some text file:
#   - tc.rst for a testcase,
#   - req.rst for a requirement,
#   - content.rst for a group
#
# * For groups, the node is further qualified by looking at the set of
#   children of the node. Despite the possibility to nest groups within
#   groups, mixing a testcase artifacts at the same level as requirement
#   artifacts is disallowed.
#
# Expected tree structure is as follows:
#
# content.rst
#   content.rst
#   req_set.rst
#     req.rst
#       tc_set.rst
#         tc.rst
#       tc.rst
#   req.rst
#
# req.txt -> req.rst
# set.txt -> tc_set.rst
# set.txt -> content.rst
# [tc.txt; test.py] -> [tc.rst; test.py]
#
# Relative to where this script reside, path to ...

# The root of the directory tree mapping qualification artifacts
# (requirements, testcases, ...):

RST_ROOTDIR = "source"

# The root of the directory where the REST documents describing
# these artifacts will reside:

ART_ROOTDIR = "../../testsuite/Qualif"

# Prefix artifact ID with this string

ART_ID_PREFIX = "TOR"

# Switch verbose messages on/off

verbose = False

# Maximum accepted directory tree depth

MAX_TREE_DEPTH = 255

# count of typed warnings

warn_stats: Dict[str, int] = {}

# replacements for variables in rst text
# the dictionary is filled by DocGenerator.generate_all before
# doing any other transpormations
VAR_REPLACEMENTS: Dict[str, str] = {}

# **********************
# ** Helper functions **
# **********************


def replace_variables(content_text: str) -> str:
    """Replace variables enclosed between | symbols."""

    for rst_var in VAR_REPLACEMENTS:
        content_text = re.sub(
            r"\|%s\|" % rst_var, VAR_REPLACEMENTS[rst_var], content_text
        )

    return content_text


def get_content(filename: str) -> str:
    """Return contents of file FILENAME as a string"""

    with open(filename, "r") as fd:
        return fd.read()


def to_title(title: str) -> str:
    """Given an entity name return a suitable string to be inserted
    in the documentation"""
    m = re.search(r"^[0-9]+_(.*)$", title)
    if m is not None:
        title = m.group(1)
    return title.replace("_", " ")


def header(text: str, pre_skip: int, post_skip: int) -> str:
    """Return the string to be used as a section header for STR,
    framed with PRE_SKIP new_lines before and POST_SKIP new_lines after"""
    return "\n" * pre_skip + text + "\n" * post_skip


def sec_header(text: str) -> str:
    """Return a Section header text to be used for section title STR"""
    return header(rest.strong(text), pre_skip=2, post_skip=2)


def subsec_header(text: str) -> str:
    """Return a Subsection header text to be used for subsection title STR"""
    return header(rest.emphasis(text), pre_skip=2, post_skip=2)


def info(text: str):
    if verbose:
        print("info: %s" % text)


def warn(text: str, msg_type: str = ""):
    if not msg_type or not any(
        re.match(pattern, msg_type) for pattern in SUPRESS_MSG_TYPES
    ):
        print("warning: %s" % text)

    if msg_type:
        if msg_type in warn_stats:
            warn_stats[msg_type] += 1
        else:
            warn_stats[msg_type] = 1


def warn_if(cond: bool, text: str, msg_type: str = ""):
    if cond:
        warn(text, msg_type)


def exit_if(p: bool, msg: str):
    if p:
        print(msg)
        sys.exit(1)


def err(text: str):
    print("error: %s" % text)
    sys.exit(1)


def print_warn_stats():
    for key in warn_stats:
        print("%s: %d" % (key, warn_stats[key]))


def err_if(p: bool, text: str):
    if p:
        print("error: %s" % text)
        sys.exit(1)


def is_functional(src: str) -> bool:
    # a resource whom content contains "-- #" is
    # a functional source.  In case of Ada Sources !!

    with open(src) as f:
        content = f.read()

    is_func = False

    for line in content.splitlines():
        if "-- #" in line:
            is_func = True
            break
    return is_func


# **************************
# ** TestCase abstraction **
# **************************

# Helper for the Directory abstraction, to encapsulate research of driver
# and functional source file sets


class TestCase:
    def __init__(self: TestCase, dir_name: str, dgen: DocGenerator):
        self.dir = dir_name
        self.dgen = dgen

        self.fnsources: set[str] = set()
        self.hlpsources: set[str] = set()
        self.drsources: set[str] = set()
        self.conspecs: set[str] = set()
        self.find_sources()

    def parent_globbing(
        self: TestCase,
        dir_name: str,
        pattern: str,
        include_start_dir: bool = True,
    ) -> Set[str]:
        """Look for src/[pattern] files in dir and its parents directory
        up to document root directory"""

        head = os.path.relpath(dir_name, self.dgen.art_rootdir)
        tail = ""
        if not include_start_dir:
            head, tail = os.path.split(head)
        files = set()
        while len(head) > 0:
            files |= set(
                glob.glob(
                    os.path.join(
                        self.dgen.art_rootdir,
                        head,
                        "src",
                        pattern,
                    )
                )
            )
            head, tail = os.path.split(head)
        return files

    def find_with_clauses(
        self: TestCase,
        dir_name: str,
        sourcefile: str,
    ) -> Set[str]:
        content = get_content(sourcefile)
        # Remove all comments
        content = "\n".join(
            [k for k in content.splitlines() if not re.match(r"\s*--", k)]
        )

        # Find all the with clauses
        matches = re.findall(r"(?:\n|;|^)\s*with\s*([^;]+)\s*;", content, re.S)
        matches = [k.replace(" ", "") for k in matches]
        matches = [k.replace(".", "-") for k in matches]

        result = set()
        for m in matches:
            result |= set(m.lower().split(","))

        # Remove packages we don't care about and probably could locate
        result -= {"support", "system"}

        file_list = set()
        for item in result:
            spec = self.parent_globbing(dir_name, item + ".ads", True)
            warn_if(
                len(spec) > 1,
                'multiple specs for unit "%s" (from %s)' % (item, sourcefile),
                "SRC_MULTIPLE_SPECS",
            )
            file_list |= spec

            body = self.parent_globbing(dir_name, item + ".adb", True)
            warn_if(
                len(body) > 1,
                'multiple bodies for unit "%s" (from %s)' % (item, sourcefile),
                "SRC_MULTIPLE_BODIES",
            )
            file_list |= body

            warn_if(
                len(body | spec) == 0,
                'no body or spec source found for unit "%s" (from %s)'
                % (item, sourcefile),
                "SRC_NO_BODY_OR_SPEC",
            )

        return file_list

    def find_closure(
        self: TestCase,
        dir_name: str,
        sourcefile: str,
    ) -> Set[str]:
        """Given an Ada source file find it's closure. Not that we ignore the
        support package"""

        result_set = self.find_with_clauses(dir_name, sourcefile)

        current_size = len(result_set)
        previous_size = 0
        while current_size > previous_size:
            previous_size = current_size
            tmp = set()
            for item in result_set:
                tmp |= self.find_with_clauses(dir_name, item)

            result_set |= tmp
            current_size = len(result_set)

        return result_set

    def find_sources(self: TestCase) -> None:
        """Locate the functional and driver sources of testcase SELF"""

        # Seek the test drivers first, and infer closure from there. Then
        # append consolidation specs to the set of drivers. We will typically
        # end up on common functional units from drivers, so use sets to
        # prevent duplicates.

        # Test drivers: search the local "src" subdir first, walk uptree
        # if no driver there.

        local_sources = set(
            glob.glob(os.path.join(self.dir, "src", "*.ad[sb]"))
        )

        self.drsources = {
            k for k in local_sources if os.path.basename(k).startswith("test_")
        }

        if len(self.drsources) == 0:
            data_names = {
                os.path.basename(k).split(".")[0] for k in local_sources
            }
            for name in data_names:
                self.drsources.update(
                    self.parent_globbing(self.dir, "test_" + name + "*.ad[sb]")
                )

        warn_if(
            len(self.drsources) == 0,
            "no driver source for testcase in %s" % self.dir,
            "SRC_NO_DRIVER_SOURCE",
        )

        # Driver Closure:

        self.fnsources = set()
        self.hlpsources = set()
        for driver in self.drsources:
            for src in self.find_closure(self.dir, driver):
                if is_functional(src):
                    self.fnsources.add(src)
                else:
                    self.hlpsources.add(src)

        warn_if(
            len(self.fnsources) == 0,
            "no functional source for testcase in %s" % self.dir,
            "SRC_NO_FUNCTIONAL_SOURCE",
        )

        # Consolidation specs. These are always local.

        self.conspecs = set()
        self.conspecs |= set(
            glob.glob(os.path.join(self.dir, "src", "cons_*.txt"))
        )


# ***************************
# ** Path Info abstraction **
# ***************************


class PathInfo:
    """Holds info about the path to the current node when walking
    a directory tree"""

    def __init__(self: PathInfo):
        self.n_req = 0  # Number of requirement expressions so far
        self.n_tc = 0  # Number of tc nodes so far
        self.depth = 0  # Depth of this node wrt root of walk operation


# **************************
# ** Node Set abstraction **
# **************************


class NodeSet:
    """Node Set abstraction, used to compute composite attributes
    for a set of nodes. We compute two categories of composite attributes:

    * The "some_" attributes tell whether some (at least one) node in set has
    such or such characteristic. These are updated as nodes are added to the
    set, via the "register_one" method.

    * The "all_" attributes tell whether all the nodes in the set has such or
    such characteristic. These are computed from the current "some_" values
    when the "sync" method is called,
    """

    def __init__(self: NodeSet, diros: List[Dir]):
        """Initialize the some_ attributes for SELF, then register
        each directory object in the DIROS sequence and sync() to update
        the all_ attributes from there."""

        self.some_reqgroup = False
        self.some_notreqgroup = False
        self.some_tcgroup = False
        self.some_nottcgroup = False
        self.some_appmat = False
        self.some_notappmat = False

        self.some_reqorset = False
        self.some_notreqorset = False
        self.some_tcorset = False
        self.some_nottcorset = False

        self.some_reqorgroup = False
        self.some_notreqorgroup = False
        self.some_tcorgroup = False
        self.some_nottcorgroup = False

        self.some_req = False
        self.some_notreq = False
        self.some_tc = False
        self.some_nottc = False
        self.some_app = False
        self.some_notapp = False
        self.some_set = False
        self.some_notset = False

        self.some_notreqset = False
        self.some_nottcset = False
        self.some_notappset = False

        self.diros: List[Dir] = []

        [self.register_one(diro) for diro in diros]
        self.sync()

    def register_one(self: NodeSet, diro: Dir):
        """Add one directory object to this set, updating the
        some_ attributes accordingly."""

        self.some_req |= diro.req
        self.some_tc |= diro.tc
        self.some_set |= diro.set
        self.some_app |= diro.app

        self.some_notreq |= not diro.req
        self.some_nottc |= not diro.tc
        self.some_notset |= not diro.set
        self.some_notapp |= not diro.app

        self.some_notreqset |= not diro.reqset
        self.some_nottcset |= not diro.tcset
        self.some_notappset |= not diro.appset

        self.some_reqgroup |= diro.reqgroup
        self.some_notreqgroup |= not diro.reqgroup

        self.some_tcgroup |= diro.tcgroup
        self.some_nottcgroup |= not diro.tcgroup

        self.some_appmat |= diro.appmat
        self.some_notappmat |= not diro.appmat

        self.some_reqorset |= diro.req | diro.reqset
        self.some_tcorset |= diro.tc | diro.tcset

        self.some_notreqorset |= not (diro.req | diro.reqset)
        self.some_nottcorset |= not (diro.tc | diro.tcset)

        self.some_reqorgroup |= diro.req | diro.reqgroup
        self.some_tcorgroup |= diro.tc | diro.tcgroup

        self.some_notreqorgroup |= not (diro.req | diro.reqgroup)
        self.some_nottcorgroup |= not (diro.tc | diro.tcgroup)

        self.diros.append(diro)
        info(
            f"registering {diro.root}: TC {diro.tc}, REQ {diro.req}, "
            "TCG {diro.tcgroup}, RG {diro.reqgroup}"
        )

    def sync(self: NodeSet):
        """Compute the all_ attributes of this set from what we know of the
        current elements in the set. All the all_ attributes are set to False
        on empty sets."""

        # Beware of queries over empty sets. All the some_ attributes
        # start False, so the all_ attributes would turn true if we're
        # not cautious

        has_diro = len(self.diros) > 0

        self.all_tc = has_diro and not self.some_nottc
        self.all_req = has_diro and not self.some_notreq
        self.all_set = has_diro and not self.some_notset
        self.all_app = has_diro and not self.some_notapp

        self.all_reqset = has_diro and not self.some_notreqset
        self.all_tcset = has_diro and not self.some_nottcset
        self.all_appset = has_diro and not self.some_notappset

        self.all_reqorset = has_diro and not self.some_notreqorset
        self.all_tcorset = has_diro and not self.some_nottcorset

        self.all_reqgroup = has_diro and not self.some_notreqgroup
        self.all_tcgroup = has_diro and not self.some_nottcgroup
        self.all_appmat = has_diro and not self.some_notappmat

        self.all_reqorgroup = has_diro and not self.some_notreqorgroup
        self.all_tcorgroup = has_diro and not self.some_nottcorgroup


# ***************************
# ** Directory abstraction **
# ***************************

# The kind of material node that the directory holds. This is used
# to determine the hyperlink and the page title texts


class DirKind:
    def __init__(self: DirKind, txthdl: str, image: str):
        self.image = image
        self.txthdl = txthdl


class dcl:
    TC = DirKind(image="tc", txthdl="Testcase")

    TCG = DirKind(image="tcg", txthdl="Testcase Group")

    REQ = DirKind(image="rq", txthdl="Requirement")

    REQG = DirKind(image="rqg", txthdl="Requirement Group")

    APP = DirKind(image="app", txthdl="Appendix Material")

    INTRO = DirKind(image="intro", txthdl="Introductory Material")

    kinds = (TC, TCG, REQ, REQG, APP, INTRO)


# DocGenerator helper to process one specific subdirectory of the TOR/TC
# hierarchy


class Dir(Artifact):
    def __init__(
        self: Dir,
        root: str,
        subdirs: List[str],
        files: List[str],
        parent: Optional[Dir],
    ):
        # Filesystem attributes for this directory

        def to_id(name: str) -> str:
            """remove leading number from name"""
            m = re.search(r"^[0-9]+_(.*)$", name)
            if m is not None:
                return m.group(1)
            else:
                return name

        self.root = root  # path to this dir
        self.subdirs = subdirs  # list of local subdir names
        self.files = files  # list of local file names
        self.pdo = parent
        self.id: str = ""

        self.name = os.path.basename(root)  # local name of this dir
        if self.pdo:
            self.id = self.pdo.id + "/" + to_id(self.name)
        else:
            self.id = (
                "/"
                + ART_ID_PREFIX
                + to_id(self.root[len(os.path.abspath(ART_ROOTDIR)) :])
            )

        # If we are at the root directory then return our documentation
        # entry point.
        if os.path.relpath(self.root, ART_ROOTDIR) == ".":
            # predefined name of the root document
            self._docfile_base_ = "index"
        else:
            # avoid name conflict with the root document
            self._docfile_base_ = self.hash()

        # Links children in the directory tree. These are
        # set as dir objects get mapped within a DirTree instance.

        # list of pruned Dir objects
        self.subdos: List[Dir] = []
        # list of child Dir obejects eactly as they appear in the
        # folder tree
        self.children: List[Dir] = []

        # NodeSet for subdos, which we'll populate when computing
        # attributes bottom-up

        self.sdset: NodeSet | None = None

        # Properties from presence of local files, better cached to prevent
        # repeated real file presence checks

        self.req: bool = "req.rst" in self.files
        self.hreq: bool = "hreq.rst" in self.files
        self.test: bool = "test.py" in self.files
        self.tc: bool = "tc.rst" in self.files
        self.htc: bool = "htc.rst" in self.files
        self.chset: bool = "content.rst" in self.files
        self.reqset: bool = "req_set.rst" in self.files
        self.tcset: bool = "tc_set.rst" in self.files
        self.app: bool = "app.rst" in self.files

        # TODO: decide later if this is required
        # currently it is to silence the warning about missing description file
        self.tcres = "test.rep" in self.files

        self.set = self.chset or self.reqset or self.tcset

        # title name, to be displayed in index tables, and sorting key.

        # The sorting key is the directory name (not title name) for starters,
        # which is meant to allow prefix characters (numbers prior to the
        # first '_') to influence sorts while not being displayed. We will
        # need to update that when bridging over nodes, to make sure that the
        # children remain grouped wrt their new parent and set of siblings.

        self.tname = to_title(self.name)
        self.sname = self.name

        # source file copies for rst toctree
        self.sources: Set[str] = set()

    # ------------------------------------------
    # -- Bottom-Up node attribute computation --
    # ------------------------------------------

    def botmup_compute_attributes(self: Dir, pathi, data):
        # Compute predicates over our set of children. We expect the children
        # attributes to be available here.
        #
        # pathi and data are note used here. The method is eventually called
        # through a generic lambda function "process" that may instantiate
        # to a method that does use them.

        self.sdset = NodeSet(diros=self.subdos)

        # Populate extra attributes on this node

        self.container = self.set or self.req or self.reqset or self.tcset

        # TC, APP or REQ SETS are nodes with consistent children (all reqs,
        # all tcs, or all apps).

        self.appset = self.set and self.sdset.all_app

        # Some mixes are legitimate, as long as the leaf item kinds (tc
        # or req) remain consistent. We call GROUPS those containers, and at
        # this stage we let the notion propagate up pretty freely:

        self.tcgroup = self.set and self.sdset.all_tcorgroup
        self.reqgroup = self.set and self.sdset.all_reqorgroup
        self.appmat = self.app or (self.set and self.sdset.all_appmat)

        # when configuration permits implicit folder types then derive type for
        # folders without an existing content file
        if self.children and not self.dfile():
            if ALLOW_UNCLASSIFIED_FOLDERS:
                if self.sdset.all_reqorgroup:
                    self.reqset = True
                elif self.sdset.all_tcorgroup:
                    self.tcset = True
            else:
                warn(
                    f"Artifact without a default content file"
                    f" in {self.full_name}",
                    "ARTIFACT_WITH_CHILDREN_NO_FILE",
                )

        # Consistency checks need to be applied elswhere to determine whether
        # such or such mix is acceptable. Consider for example the difference
        # in consistency between
        #
        # 1) either:
        #    set/tc
        #       /tc
        #       /tc
        #
        #    or:
        #    set/set/tc
        #           /tc
        #       /set/tc
        #           /tc
        #
        # (only allow sets of TCs, or sets of sets)
        #
        # 2) or possibly
        #    set/set/tc
        #           /tc
        #       /tc
        #
        # (allow sets of either tc or set)

        # Now compute the node KIND, which conveys how we want the node to be
        # referenced (in index table hyperlinks) and titled. This is not meant
        # to be representative of structural properties. REQ nodes often are
        # tcset or tcgroup as well, for example.

        self.kind = (
            dcl.TC
            if self.tc
            else (
                dcl.REQ
                if self.req
                else (
                    dcl.TCG
                    if self.tcgroup
                    else (
                        dcl.REQG
                        if self.reqgroup
                        else dcl.APP if self.appmat else dcl.INTRO
                    )
                )
            )
        )

    def dfile(self: Dir, path: bool = False) -> str | None:
        """Name of the text file which holds the meat of this dir/artifact"""

        base = (
            "tc.rst"
            if self.tc
            else (
                "content.rst"
                if self.chset
                else (
                    "req_set.rst"
                    if self.reqset
                    else (
                        "tc_set.rst"
                        if self.tcset
                        else (
                            "req.rst"
                            if self.req
                            else (
                                "htc.rst"
                                if self.htc
                                else (
                                    "hreq.rst"
                                    if self.hreq
                                    else "app.rst" if self.app else None
                                )
                            )
                        )
                    )
                )
            )
        )

        return os.path.join(self.root, base) if base and path else base

    @property
    def ext_tcset(self: Dir) -> bool:
        self.tcset = self.tcset or (
            not self.dfile()
            and (self.sdset.some_tcorset if self.sdset else False)
        )

        return self.tcset

    def dtext(self: Dir) -> str:
        if self.dfile() is None:
            # This means that dtext was called on an artifact with unknown type
            if ALLOW_UNCLASSIFIED_FOLDERS:
                warn(
                    "Artifact without a default content file"
                    " in %s" % self.full_name
                )
                return ""
            else:
                err(
                    "Artifact without a default content file in"
                    " %s" % self.full_name
                )
                return ""
        else:
            file = self.dfile(path=True)
            content = ""
            if file and os.path.isfile(file):
                content = get_content(file)
            else:
                warn("Content file %s does not exist." % file)
                if self.tcset:
                    # the folder contains test cases, construct a default tcset
                    content = (
                        self.id
                        + """

.. qmlink:: TCIndexImporter

   *
"""
                    )
            # ensure that content starts with title
            if file and file.endswith(".rst"):
                title_prefix = ""
                title = ""
                body = ""
                title_candidate = ""

                for line in content.splitlines():
                    if title:
                        # The title is identified.
                        # Add all the remaining lines to the body
                        body += "\n" + line
                    else:
                        if title_candidate:
                            # check the line following the title candidate
                            clean_line = line.strip()
                            if not clean_line:
                                # empty line after the first row. Promote the
                                # first row to be the "title".
                                title = title_candidate
                            elif (
                                not re.search(r"[^=]", clean_line)
                                or not re.search(r"[^\*]", clean_line)
                                or not re.search(r"[^#]", clean_line)
                            ):
                                # first level title -- the next line is
                                # entirely composed of one of the title markers
                                title = title_candidate + "\n" + clean_line
                            else:
                                # the following line contains arbitrary text
                                # give a warning and stop the search
                                warn(f"No title found in {file}")
                                break
                        else:
                            # no lines with content yet
                            # if we find one then that is treated as
                            # a title candidate.
                            if not line or line.startswith(".. "):
                                # preserve empty lines and rst directives
                                title_prefix += line + "\n"
                            else:
                                # any other text is considered as title
                                title_candidate = line.strip()

                if title:
                    content = title + "\n\n" + self.parent_ref + "\n\n" + body
                else:
                    content = content + "\n\n" + self.parent_ref
            else:
                content = content + "\n\n" + self.parent_ref

            return content

    def docfile(self: Dir, no_extension: bool = True) -> str:
        """Filename for the generated RST file"""

        if no_extension:
            return self._docfile_base_
        else:
            return self._docfile_base_ + ".rst"

    def get_children(self: Dir) -> Sequence[Dir]:
        """Return list of subartifacts"""
        return self.children

    def get_name(self: Dir) -> str:
        return self.name

    def rest_doc_ref(self: Dir, id_as_title: bool = False) -> str:
        """Returns a sphinx :doc: on a.docfile()

        By default uses the title string as link label.
        id_as_title=True replaces it with id string.
        """

        return writer.qmref(
            self.docfile(True),
            replace_variables(self.title()) if not id_as_title else self.id,
        )

    @property
    def full_name(self: Dir) -> str:
        return self.root

    @property
    def relatives(self: Dir) -> Sequence[Dir]:
        """
        The list of sub-artifacts that are relative to self.
        """
        return self.children

    @property
    def relative_to(self) -> Optional[Dir]:
        """
        The instance this artifact is relative to.
        """
        return self.pdo

    @property
    def full_id(self: Dir) -> str:
        """Return an id prefixed with item kind"""
        return "**" + self.kind.txthdl.upper() + "** " + self.id

    @property
    def parent_ref(self: Dir) -> str:
        """Return link to the parent object.

        Include artifact type when parent is requirement.
        """
        if self.pdo:
            return (
                "**Parent"
                + ((" " + self.pdo.kind.txthdl) if self.pdo.req else "")
                + "** "
                + self.pdo.rest_doc_ref()
            )
        else:
            return ""


# ********************************
# ** Directory Tree abstraction **
# ********************************

# Helper to manage dirname -> dirobject associations and establish
# parent/children relationships over dir objects, assuming the tree
# is walked top down.

# Values to denote possible ways to walk a tree and possible actions to take
# at every directory node when walking the tree


topdown, botmup = range(2)

dirProcess, dirSkip, dirCutPre, dirCutPost = range(4)
# dirProcess: process this node and walk children
# dirCutPre:  don't process this node and don't walk children
# dirCutPost: process this node and don't walk children
# dirSkip:    skip processing for this node, walk children nevertheless

im = {
    dirSkip: "dirSkip",
    dirProcess: "dirProcess",
    dirCutPre: "dirCutPre",
    dirCutPost: "dirCutPost",
}


class DirTree:
    def __init__(self: DirTree, roots: List[Dir]):
        self.roots = roots

    # ------------------------------------------------------------
    # -- Tree walking facilities, once we're past the first doc --
    # -- generation pass, all the parent children links are set --
    # ------------------------------------------------------------

    # Local facilities. Beware that walks are used to compute directory
    # object attributes, so these can't be relied upon.

    class WalkInfo:
        def __init__(
            self: DirTree.WalkInfo,
            data: DirTree.WalkInfo | None,
            pathi: PathInfo,
            process: Callable | None,
            ctl: Callable,
            mode: int,
        ):
            self.process = process if process else lambda diro, pathi, wi: None
            self.ctl = ctl
            self.mode = mode
            self.pathi = pathi
            self.data = data

    def __enter(self: DirTree, diro: Dir, wi):
        if diro.req:
            wi.pathi.n_req += 1
        if diro.tc:
            wi.pathi.n_tc += 1
        return wi.ctl(diro, wi.pathi, wi.data)

    def __exit(self: DirTree, diro: Dir, wi: DirTree.WalkInfo):
        if diro.req:
            wi.pathi.n_req -= 1
        if diro.tc:
            wi.pathi.n_tc -= 1

    def __visit(self: DirTree, diro: Dir, wi: DirTree.WalkInfo):
        ctl = self.__enter(diro, wi)

        process_this = ctl not in [dirSkip, dirCutPre]
        visit_children = ctl not in [dirCutPre, dirCutPost]

        if process_this and wi.mode == topdown:
            wi.process(diro, wi.pathi, wi.data)

        if visit_children:
            wi.pathi.depth += 1
            [self.__visit(subdo, wi) for subdo in diro.subdos]
            wi.pathi.depth -= 1

        if process_this and wi.mode == botmup:
            wi.process(diro, wi.pathi, wi.data)

        self.__exit(diro, wi)

    def __default_ctl(self: DirTree, diro: Dir, pathi, widata):
        return dirProcess

    # Exposed facilities.

    def walk(
        self: DirTree,
        mode: int,
        process: Callable,
        data: WalkInfo | None = None,
        ctl: Callable | None = None,
    ):
        if ctl is None:
            ctl = self.__default_ctl

        [
            self.__visit(
                diro=diro,
                wi=DirTree.WalkInfo(
                    process=process,
                    pathi=PathInfo(),
                    data=data,
                    mode=mode,
                    ctl=ctl,
                ),
            )
            for diro in sorted(self.roots, key=Dir.get_name)
        ]

    # ---------------------------------------
    # -- Tree filtering/pruning facilities --
    # ---------------------------------------

    def rprune(self: DirTree, namelist: List[str]):
        """prune all subdirs of self that are do *not* lead to any dir
        in NAMELIST"""

        class WalkInfo(DirTree.WalkInfo):
            def __init__(self: WalkInfo, tokeep: List[str]):
                self.tokeep = tokeep
                self.toprune: List[Dir] = []

        def prunectl(diro: Dir, pathi, wi: WalkInfo) -> int:
            # See if diro is within or beyond any of our dirs to keep.
            # Keep going (skip) if within. Cut search if beyond.

            # Append "/" to items to prevent "bla/C" from being considered
            # a directory prefix of "bla/Common"

            rootdir = diro.root + os.sep

            for tokeep in wi.tokeep:
                dirtokeep = tokeep + os.sep
                if os.path.commonprefix((rootdir, dirtokeep)) == rootdir:
                    return (
                        dirSkip if len(rootdir) < len(dirtokeep) else dirCutPre
                    )

            # diro is for sure not within or down any of our dirs to keep.
            # register for actual pruning and don't search past it.

            wi.toprune.append(diro)
            return dirCutPre

        # Walk this tree to compute the list of directories to prune, then do
        # prune for real.  We don't effectively prune nodes during the walk to
        # prevent mishaps caused by changes to our internal datastructures
        # while they are being processed.

        wi = WalkInfo(tokeep=namelist)
        self.walk(
            mode=topdown,
            process=lambda diro, pathi, wi: None,
            ctl=prunectl,
            data=wi,
        )

        for diro in wi.toprune:
            if diro.pdo:
                diro.pdo.subdos.remove(diro)

    # ---------------------
    # -- Sorting entries --
    # ---------------------

    def sort(self: DirTree):
        """Sort the list of subdirectory objects registered for each tree
        node, to influence processing order in tree walks (hence insertion
        order in auto-indexes)"""

        def sort_subdos(diro, pathi, wi):
            # Sort subdirectories of DIRO. Arrange to get
            #
            # * All the non containers first, to make sure that
            #
            #     sub/x
            #     y
            #
            #   gets out as
            #
            #     y     bla
            #     sub   blo
            #     x     blu
            #
            #   and not as
            #
            #     sub  blo
            #     x    blu
            #     y    bla
            #
            #   where y would be perceived as a child of "sub"
            #
            #  * Those for which a local key in their respective key order,
            #    all before or after those without explicit keys depending
            #    on the "default" entry,
            #
            #  * In every subcase, sort by subdirectory name

            # Fetch the local { subdirname: key } dictionary, if any

            kfile = os.path.join(diro.root, "keys.txt")
            keys = (
                json.loads(get_content(kfile))
                if os.path.exists(kfile)
                else {"default": 0}
            )

            # Then sort according to our three criteria of interest

            diro.subdos.sort(
                key=lambda subdo: (
                    subdo.container,
                    (
                        keys[subdo.sname]
                        if subdo.sname in keys
                        else keys["default"]
                    ),
                    subdo.sname,
                )
            )

        self.walk(mode=botmup, process=sort_subdos)

    # -----------------------------------------
    # -- Checking directory tree consistency --
    # -----------------------------------------

    def check_local_consistency(self: DirTree, diro: Dir, pathi: PathInfo):
        """Perform checks on the files present in DIRO"""

        warn_if(
            not (
                diro.set
                or diro.req
                or diro.tc
                or diro.app
                or diro.htc
                or diro.hreq
                or diro.tcres
            ),
            "missing description text at %s" % diro.root,
            "MISSING_DESCRITPTION_TEXT",
        )
        rst_count = len([f for f in diro.files if f.endswith(".rst")])
        warn_if(
            diro.req and rst_count > 1,
            "req.rst not alone in %s" % diro.root,
        )
        warn_if(
            diro.chset and rst_count > 1,
            "content.rst not alone in %s" % diro.root,
        )
        warn_if(
            diro.tc and not diro.test,
            "tc.rst without test.py in %s" % diro.root,
        )
        warn_if(
            diro.test and not diro.tc and not diro.htc,
            "test.py without tc.rst in %s" % diro.root,
        )

        warn_if(
            (diro.tc or diro.tcgroup) and pathi.n_req < 1,
            "tc or set without req uptree at %s" % diro.root,
        )

        warn_if(
            diro.req and pathi.n_req > 1,
            "nested req (%d up) at %s" % (pathi.n_req - 1, diro.root),
        )

        warn_if(
            diro.req and pathi.n_tc > 0,
            "req with tc uptree at %s" % diro.root,
        )

    def check_downtree_consistency(self, diro, pathi):
        """Perform checks on the relationships DIRO and its children"""

        warn_if(
            (diro.req or diro.set) and not diro.subdos,
            "missing subdirs for artifact at %s" % diro.root,
        )

        if not diro.subdos:
            return

        # Warn on structural inconsistencies

        warn_if(
            diro.req and not diro.sdset.all_tcorgroup,
            "inconsistent subdirs down req.rst at %s" % diro.root,
        )

        warn_if(
            diro.req
            and not (diro.sdset.all_reqorgroup or diro.sdset.all_tcorgroup),
            "missing testcases for leaf req in %s" % diro.root,
        )

    def topdown_check_consistency(
        self: DirTree, diro: Dir, pathi: PathInfo, data
    ):
        self.check_local_consistency(diro, pathi)
        self.check_downtree_consistency(diro, pathi)

    def check_consistency(self: DirTree):
        self.walk(mode=topdown, process=self.topdown_check_consistency)


class DirTree_FromPath(DirTree):

    def __init__(self: DirTree_FromPath, rootp: str):
        DirTree.__init__(self, roots=[])

        # First compute the tree of directory objects starting at path ROOTP,
        # setting up for each the .pdo link to parent and the .subdos list of
        # children. This is achieved by performing a top down walk of the os
        # directory structure.

        self.dir = {}  # dir-name -> dir-object dictionary
        [
            self.topdown_map(dirname, subdirs, files)
            for (dirname, subdirs, files) in os.walk(os.path.abspath(rootp))
        ]

        # Arrange for the intermediate directories just there for file
        # organisation purposes to disappear from our tree representation

        self.do_cross_overs()

        # Then compute exta node attributes, once the tree of internal
        # directory objects is setup.

        self.compute_attributes()

    def topdown_map(
        self: DirTree_FromPath,
        dirname: str,
        subdirs: List[str],
        files: List[str],
    ):
        # Map directory DIRNAME into our dictionary and set up links to/from
        # its parent directory, if any. We're called along a topdown walk, so
        # we have mapped the parent directory already if there is one.

        # Ignore some subdirectories
        for d in subdirs.copy():
            if (
                d in (".svn", "mylib", "App", "__pycache__")
                or "src" in d
                or "obj" in d
                or d.startswith("tmp_")
                or d.startswith("s_")
                or d.startswith("d_")
                or d.startswith("m_")
                or d.startswith("u_")
                or os.path.exists(os.path.join(dirname, d, ".tor_ignore"))
            ):
                subdirs.remove(d)

        # Find out its parent object by name. If there's no parent object,
        # this dir is a root. Setup the links otherwise.

        parentname = os.path.dirname(dirname)

        if parentname not in self.dir:
            parento = None
        else:
            parento = self.dir[parentname]

        # Map a new object for this dir ...

        diro = Dir(root=dirname, subdirs=subdirs, files=files, parent=parento)

        self.dir[dirname] = diro

        # Set links to parent

        if not parento:
            self.roots.append(diro)
        else:
            parento.subdos.append(diro)
            parento.children.append(diro)

    # --------------------------------------------------------------
    # -- Abstract away nodes that were introduced only for source --
    # -- sharing purposes, and that should not introduce an extra --
    # -- layer in the qualification material.                     --
    # -- -----------------------------------------------------------

    def __do_bridge_over(self: DirTree_FromPath, diro: Dir):
        """Arrange for DIRO to disappear from the dir tree structure
        it is in."""

        print("======== Bridging over " + diro.root)

        # Remove this diro from the list of children of its parent,
        # and update each of this diro's children to have its parent
        # as a new parent instead of this diro itself.

        # Turn:
        #                  P          <----- PARENT
        #                  |
        #                 DIRO        <----- TO VANISH
        #                  |
        #            +---+---+---+
        #            |   |   |   |
        #           c1   c2  c3 c4    <---- CHILDREN
        #
        # into:
        #
        #                  P          <----- PARENT
        #                  |
        #            +---+---+---+
        #            |   |   |   |
        #           c1   c2  c3  c4   <---- CHILDREN

        if not diro.pdo:
            err(f"malformed direcotry object: {diro.get_name()}")
            return

        diro.pdo.subdos.remove(diro)

        for subdo in diro.subdos:
            diro.pdo.subdos.append(subdo)
            subdo.pdo = diro.pdo

            subdo.tname = ".".join([diro.tname, subdo.tname])
            subdo.sname = ".".join([diro.sname, subdo.sname])

    class WalkInfo(DirTree.WalkInfo):
        def __init__(self: DirTree_FromPath.WalkInfo):
            self.tobridge = []

    def __decide_cross_over(
        self: DirTree_FromPath,
        diro: Dir,
        pathi: PathInfo,
        wi: DirTree_FromPath.WalkInfo,
    ):
        """Add DIRO to WI.tobridge if DIRO ought to be removed from the
        tree."""

        file = diro.dfile(path=True)
        if file is None:
            file = ""

        if diro.set and os.path.getsize(file) == 0:
            wi.tobridge.append(diro)

    def do_cross_overs(self: DirTree_FromPath):
        """Remove (bridge links over) intermediate nodes that should not
        produce intermediate layers in the final document."""

        # Collect the set of nodes to remove first, then remove each one in
        # turn. Removing while we're walking the tree is, mm, hazardous.

        wi = DirTree_FromPath.WalkInfo()
        self.walk(mode=topdown, process=self.__decide_cross_over, data=wi)

        [self.__do_bridge_over(diro) for diro in wi.tobridge]

    # ---------------------------------------------
    # -- Computing tree/node attributes, before  --
    # -- more sophisticated walks can take place --
    # ---------------------------------------------

    def compute_attributes(self: DirTree_FromPath):
        self.walk(mode=botmup, process=Dir.botmup_compute_attributes)


# ************************
# ** Document Generator **
# ************************

icLink, icNid, icBrief = range(3)


class DocGenerator(object):
    def __init__(
        self: DocGenerator,
        art_rootdir: str,
        rst_rootdir: str,
        options: optparse.Values,
    ):

        # Command line options
        self.o = options

        # Root of the directory tree where the qualification artifacts
        # are to be found:
        self.art_rootdir = os.path.abspath(art_rootdir)

        # Root of the directory tree where the generated document sources
        # are to be produced:
        self.rst_rootdir = os.path.abspath(rst_rootdir)

        if os.path.exists(self.rst_rootdir):
            if self.o.force:
                warn("Deleting an existing directory %s" % self.rst_rootdir)
                for root, dirs, files in os.walk(
                    self.rst_rootdir, topdown=False
                ):
                    for name in files:
                        os.remove(os.path.join(root, name))
                    for name in dirs:
                        os.rmdir(os.path.join(root, name))
                os.rmdir(self.rst_rootdir)
            else:
                err(
                    "Directory %s already exists. "
                    "Use --force to delete it or clean up filesystem manually."
                    % self.rst_rootdir
                )

        os.mkdir(self.rst_rootdir)

        self.resource_list: Set[str] = set()

        # current output file descriptor, while walking the tor/tc tree
        self.ofd: IO[Any]

    def register_resources(self: DocGenerator, rset: Set[str]):
        self.resource_list |= rset

    def docpath_to(self: DocGenerator, filename: str) -> str:
        return os.path.join(self.rst_rootdir, filename)

    def get_docfile(
        self: DocGenerator, diro: Dir, must_exist: bool = False
    ) -> str | None:
        """Return full path to output file of diro.
        If must_exist is True, then return the name only for existing files
        """
        name = self.docpath_to(diro.docfile(no_extension=False))
        return name if not must_exist or os.path.isfile(name) else None

    def file2docfile(
        self: DocGenerator, filename: str, must_exist: bool = False
    ) -> str | None:
        """Return the associated filename for a given path"""
        docfile = os.path.relpath(filename, self.art_rootdir)
        # If we are at the root directory then return our documentation
        # entry point.
        if docfile == ".":
            return "index.rst"

        docfile = docfile.replace("/", "_").replace("\\", "_") + ".rst"
        return docfile if not must_exist or os.path.isfile(docfile) else None

    def ref(self: DocGenerator, name: str) -> str:
        """Transform string NAME into another string suitable to be used as
        index name"""
        result = os.path.relpath(name, self.art_rootdir)
        return result.replace("/", "_").replace("\\", "_").replace(".", "_")

    # ---------------------------------------------
    # -- Generating doc contents for a directory --
    # ---------------------------------------------

    def contents_from(self: DocGenerator, diro: Dir) -> str:
        """Fetch descriptive text form the file corresponding to diro.
        Append a TC index to TC set descriptions that don't have one."""

        # Fetch the descriptive text either through a specialized importer, or
        # through our own

        importer = default_importer(diro)
        # if isinstance(importer, DefaultImporter):
        #     res = diro.to_rest()
        # else:
        res = importer.to_rest(diro)
        res = replace_variables(res)
        res = self.process_qmlink(diro, res)

        return res

    def process_qmlink(
        self: DocGenerator, diro: Dir, content_text: str
    ) -> str:
        """extract ´.. qmlink::´ directives and replace their content"""

        res = ""
        in_qmlink = False
        subartifacts: Sequence[Artifact] = []
        importer: ArtifactImporter | None = None
        for line in content_text.splitlines():
            if in_qmlink:
                stripped_line = line.rstrip()
                if stripped_line:
                    indent_len = len(stripped_line) - len(
                        stripped_line.strip()
                    )
                    if indent_len == 0 or stripped_line.startswith(".. "):
                        # the indent declined or a new block
                        # end the current block
                        if importer:
                            qmlink_rst, links = importer.qmlink_to_rest(
                                diro, subartifacts
                            )
                            diro.subdos += [
                                item
                                for item in subartifacts
                                if item not in diro.subdos
                            ]
                            res += "\n" + qmlink_rst

                        in_qmlink = False
                        subartifacts = []
                        importer = None

                    elif stripped_line.strip() == "*":
                        subartifacts.extend(
                            sorted(diro.get_children(), key=Dir.get_name)
                        )
                    else:
                        a = diro.get_child(stripped_line)
                        if a:
                            subartifacts.append(a)
                        else:
                            logger.log_error(
                                "Cannot locate artifact '"
                                + stripped_line
                                + "'"
                            )

            if ArtifactImporter.is_qmlink(line):
                in_qmlink = True
                importer = resolve_importer(line)
                if not importer:
                    logger.log_error(
                        "Cannot locate importer for '" + line + "'"
                    )
            elif not in_qmlink:
                res += "\n" + line

        if importer and subartifacts:
            # rst section in the end of the document
            qmlink_rst, links = importer.qmlink_to_rest(diro, subartifacts)
            diro.subdos += [
                item for item in subartifacts if item not in diro.subdos
            ]
            res += "\n" + qmlink_rst

        return res

    def gen_tc_section(self: DocGenerator, diro: Dir):
        """Generate the TestCase Sources section, and register the
        sources as resources

        TODO: This should really belong in an artifatc importer??
        """

        def restore_extension(name: str) -> str:
            """restores .ad[bs] extension in name"""
            if name.endswith("_ads"):
                return name[:-4] + ".ads"
            elif name.endswith("_adb"):
                return name[:-4] + ".adb"
            elif name.endswith("_txt"):
                return name[:-4] + ".txt"
            else:
                return name

        tco = TestCase(dir_name=diro.root, dgen=self)

        if (
            not tco.fnsources
            and not tco.drsources
            and not tco.hlpsources
            and not tco.conspecs
        ):
            # no sources, nothing to process
            return

        headers = ["Functional Sources", "Drivers Sources", "Helpers Sources"]

        func_list = sorted(tco.fnsources)
        driver_list = sorted(tco.drsources)
        helper_list = sorted(tco.hlpsources)

        func_list_ref = [self.ref(d) for d in func_list]
        driver_list_ref = [self.ref(d) for d in driver_list]
        helper_list_ref = [self.ref(d) for d in helper_list]

        func_list_qmref = [":ref:`%s`" % d for d in func_list_ref]
        driver_list_qmref = [":ref:`%s`" % d for d in driver_list_ref]
        helper_list_qmref = [":ref:`%s`" % d for d in helper_list_ref]

        if tco.conspecs:
            consolidation_list_ref = [
                self.ref(d) for d in sorted(tco.conspecs)
            ]
            consolidation_list_qmref = [
                ":ref:`%s`" % d for d in consolidation_list_ref
            ]

            headers += ["Consolidation Sources"]

            for_table_qmref = zip_longest(
                func_list_qmref,
                driver_list_qmref,
                helper_list_qmref,
                consolidation_list_qmref,
                fillvalue="",
            )

        else:
            consolidation_list_ref = []
            for_table_qmref = zip_longest(
                func_list_qmref,
                driver_list_qmref,
                helper_list_qmref,
                fillvalue="",
            )

        html_content = writer.only(
            writer.csv_table(list(for_table_qmref), headers), "html"
        )

        self.ofd.write(html_content)

        diro.sources = {
            restore_extension(f)
            for f in (
                func_list_ref
                + driver_list_ref
                + helper_list_ref
                + consolidation_list_ref
            )
            if f
        }

        self.register_resources(
            tco.fnsources | tco.drsources | tco.hlpsources | tco.conspecs
        )

    # --------------------------------------
    # -- Tailored directory tree instance --
    # --------------------------------------

    def art_dirtree_for(
        self: DocGenerator, chapdirs: List[str]
    ) -> DirTree_FromPath:
        dirtree = DirTree_FromPath(rootp=self.art_rootdir)

        dirtree.rprune(
            [os.path.join(self.art_rootdir, dirname) for dirname in chapdirs]
        )

        dirtree.check_consistency()
        dirtree.sort()

        return dirtree

    # ---------------------------
    # -- generate doc chapters --
    # ---------------------------

    ALT_TITLES = {"Qualif": "GNATcoverage TORs"}

    def __gen_doc_contents(self: DocGenerator, diro: Dir, pathi, wi):
        """Generate the RST file for the DIRO artifact/directory-object, as
        part of a dirtree walk carrying a general DirTree.WalkInfo object WI
        and path info PATHI"""

        filename = self.get_docfile(diro)
        if not filename:
            err(f"Could not get filename for {diro.get_name()}")
            return
        if not os.path.isdir(os.path.dirname(filename)):
            os.makedirs(os.path.dirname(filename))
        self.ofd = open(filename, "w")

        if diro.dfile():
            self.ofd.write(self.contents_from(diro))

        if diro.tc:
            self.gen_tc_section(diro)

        # close the document to measure the size
        # that is more robust than trying to keep track of all the write
        # operations and we can affort the small overhead of reopening it
        # once more
        self.ofd.close()

        # remove the document if no content was generated
        if os.path.getsize(filename) == 0:
            os.remove(filename)
        else:
            # reopen the file to add toc
            self.ofd = open(filename, "a")
            self.ofd.close()

    def generate_chapters(self: DocGenerator):
        self.art_dirtree.walk(mode=topdown, process=self.__gen_doc_contents)

    # ----------------------------
    # -- generate resource file --
    # ----------------------------

    def generate_resources(self: DocGenerator):
        fd = open(self.docpath_to("resources.rst"), "w")
        fd.write(rest.chapter("Resources"))
        fd.write(
            rest.toctree(
                [
                    file
                    for d in self.resource_list
                    if (file := self.file2docfile(d, must_exist=True))
                ]
            )
        )
        fd.close()

        for r in self.resource_list:
            filename = self.file2docfile(r)
            if not filename:
                err(f"Could not get doc filename for {r}")
                return
            fd = open(self.docpath_to(filename), "w")
            fd.write("\n.. _%s:\n\n" % self.ref(r))
            fd.write(rest.section(os.path.basename(r)))
            fd.write(rest.code_block(get_content(r), "ada"))
            fd.close()

    # ---------------------------------------------
    # -- generate all the document items,        --
    # -- checking tree consistency along the way --
    # ---------------------------------------------

    def generate_all(self: DocGenerator):
        # Compute the set of subdirs/chapters our document ought to include.
        # This is the explicit list when one was provided, and a set of common
        # subdirs + those of relevance for the requested criterion otherwise.

        crit_subdirs_for = {
            "doC": ["stmt"],
            "doB": ["stmt", "decision"],
            "doA": ["stmt", "decision", "mcdc"],
        }

        this_subdirs = (
            self.o.chapdirs.split(",")
            if self.o.chapdirs
            else (
                ["OpEnviron", "Appendix", "Common"]
                + [
                    os.path.join("Ada", csd)
                    for csd in crit_subdirs_for[self.o.dolevel]
                ]
            )
        )

        VAR_REPLACEMENTS.update(
            get_var_replacements(do_level=self.o.dolevel, doc_id=tor_doc_id)
        )

        self.art_dirtree = self.art_dirtree_for(this_subdirs)

        self.generate_chapters()
        self.generate_resources()

        print_warn_stats()


# The main of the script

valid_dolevels = ("doA", "doB", "doC")

if __name__ == "__main__":
    op = optparse.OptionParser(usage="%prog <options>")

    op.add_option(
        "--dolevel",
        dest="dolevel",
        default=None,
        type="choice",
        choices=valid_dolevels,
        help=("Target DO178 qualification level."),
    )

    op.add_option(
        "--chapdirs",
        dest="chapdirs",
        default=None,
        help=("Comma separated list of chapter directories to consider"),
    )

    op.add_option(
        "--force",
        dest="force",
        action="store_true",
        default=False,
        help=("Delete existing output directory if it exists and continue"),
    )

    op.add_option(
        "--verbose",
        "-v",
        dest="verbose",
        action="store_true",
        default=False,
        help=("Show detailed info messages"),
    )
    (options, args) = op.parse_args()

    exit_if(
        not options.chapdirs and not options.dolevel,
        "A DO level or a set of chapter dirs must be specified",
    )

    exit_if(
        options.chapdirs and options.dolevel,
        "A DO level may not be specified together with an explicit "
        "set of chapter dirs.",
    )

    verbose = options.verbose

    mygen = DocGenerator(
        art_rootdir=ART_ROOTDIR, rst_rootdir=RST_ROOTDIR, options=options
    )

    mygen.generate_all()
