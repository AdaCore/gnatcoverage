#!/usr/bin/env python

import os
import rest
import glob
import re
import sys
import json

DOC_DIR = "source"
ROOT_DIR = "../../../testsuite/Qualif"

# **********************
# ** Helper functions **
# **********************

def get_content(filename):
    """Return contents of file FILENAME as a string"""

    fd = open(filename, 'r')
    content = fd.read()
    fd.close()
    return content

def copy(l):
    """Return a copy of list L"""
    return [item for item in l]

def to_title(str):
    """Given an entity name return a suitable string to be insterted
    in the documentation"""
    m = re.search(r'^[0-9]+_(.*)$', str)
    if m is not None:
        str = m.group(1)
    return str.replace('_', ' ')

def header(str, pre_skip, post_skip):
    """Return the string to be used as a section header for STR,
    framed with PRE_SKIP new_lines before and POST_SKIP new_lines after"""
    return '\n' * pre_skip + str + '\n' * post_skip

def sec_header(str):
    """Return a Section header text to be used for section title STR"""
    return header(rest.strong(str), pre_skip=2, post_skip=2)

def subsec_header(str):
    """Return a Subsection header text to be used for subsection title STR"""
    return header(rest.emphasis(str), pre_skip=2, post_skip=2)

def warn(text):
    print "warning: %s" % text

def warn_if(cond, text):
    if cond: warn(text)


# **************************
# ** TestCase abstraction **
# **************************

# Helper for the Directory abstraction, to encapsulate research of driver
# and functional source file sets

class TestCase:
    def __init__(self, dir, dgen):
        self.dir  = dir
        self.dgen = dgen

        self.fnsources = None
        self.drsources = None
        self.find_sources()

    def parent_globbing(self, dir, pattern, include_start_dir=True):
        """Look for src/[pattern] files in dir and its parents directory
        up to document root directory"""
        head = os.path.relpath(dir, self.dgen.root_dir)
        tail = ''
        if not include_start_dir:
            head, tail = os.path.split(head)
        files = set([])
        while len(head) > 0:
            files |= set(
                glob.glob(
                    os.path.join(self.dgen.root_dir, head, 'src', pattern))
                )
            head, tail = os.path.split(head)
        return files

    def find_with_clauses(self, dir, sourcefile):
        content = get_content(sourcefile)
        # Remove all comments
        content = '\n'.join([k for k in content.splitlines() \
                             if not re.match('\s*--', k)])

        # Find all the with clauses
        matches = re.findall(r'(?:\n|;|^)\s*with\s*([^;]+)\s*;', content, re.S)
        matches = [k.replace(' ', '') for k in matches]
        matches = [k.replace('.', '-') for k in matches]

        result = []
        [result.extend (m.lower().split(',')) for m in matches]
        result = set(result)

        # Remove packages we don't care about and probably could locate
        result -= set(['support', 'system'])

        file_list = set([])
        for item in result:

            spec = self.parent_globbing(dir, item + '.ads', True)
            warn_if (len(spec) > 1, 'multiple specs for unit "%s"' % item)
            file_list |= spec

            body = self.parent_globbing(dir, item + '.adb', True)
            warn_if (len(body) > 1, 'multiple bodies for unit "%s"' % item)
            file_list |= body

            warn_if (len(body | spec) == 0,
                'no body or spec source found for unit "%s" (from %s)'
                     % (item, sourcefile))

        return file_list

    def find_closure(self, dir, sourcefile):
        """Given an Ada source file find it's closure. Not that we ignore the
        support package"""

        result_set = self.find_with_clauses(dir, sourcefile)

        current_size = len(result_set)
        previous_size = 0
        while current_size > previous_size:

            previous_size = current_size
            tmp = set([])
            for item in result_set:
                tmp |= self.find_with_clauses(dir, item)

            result_set |= tmp
            current_size = len(result_set)

        return result_set

    def find_sources(self):
        """Locate the functional and driver sources of testcase SELF"""

        # Seek the test drivers first, and infer closure from there. Then
        # append consolidation specs to the set of drivers. We will typically
        # end up on common functional units from drivers, so use sets to
        # prevent dupicates.

        # Test drivers: search the local "src" subdir first, walk uptree
        # if no driver there.

        local_sources = set(
            glob.glob(os.path.join(self.dir, 'src', '*.ad[sb]')))

        self.drsources = set(
            [k for k in local_sources
             if os.path.basename(k).startswith('test_')])

        if len(self.drsources) == 0:
            data_names = set(
                [os.path.basename(k).split('.')[0] for k in local_sources])
            [self.drsources.update(
                    self.parent_globbing(self.dir, 'test_'+name+'*.ad[sb]'))
             for name in data_names]

        warn_if (len(self.drsources) == 0,
            'no driver source for testcase in %s' % self.dir)

        # Driver Closure:

        self.fnsources = set([])
        [self.fnsources.update(self.find_closure(self.dir, driver))
         for driver in self.drsources]

        warn_if (len(self.fnsources) == 0,
            'no functional source for testcase in %s' % self.dir)

        # Consolidation specs. These are always local.

        self.conspecs = set([])
        self.conspecs |= set(
            glob.glob(os.path.join(self.dir, 'src', 'cons_*.txt')))

# ***************************
# ** Path Info abstraction **
# ***************************

# Holds info about the path to the current node when walking
# a directory tree

class PathInfo:
    def __init__(self):
        self.n_req = 0  # Number of requirement expressions so far
        self.n_tc = 0   # Number of tc nodes so far
        self.depth = 0  # Depth of this node wrt root of walk operation

# ***************************
# ** Directory abstraction **
# ***************************

class DirKind:
    def __init__ (self, txthdl, image):
        self.image  = image
        self.txthdl = txthdl

class dcl:
    TC = DirKind (
        image="tc", txthdl = "Testcase")
    TCSET = DirKind (
        image="tcg", txthdl = "Testcase Group")
    REQ = DirKind (
        image="rq", txthdl = "Requirement")
    REQSET = DirKind (
        image="rqg", txthdl = "Requirement Group")
    INTRO = DirKind (
        image="intro", txthdl = "Introductory Material")

    kinds = (TC, TCSET, REQ, REQSET, INTRO)

# DocGenerator helper to process one specific subdirectory of the TOR/TC
# hierarchy

class Dir:
    def __init__(self, root, subdirs, files):

        # Filesystem attributes for this directory

        self.root    = root    # path to this dir
        self.subdirs = subdirs # list of local subdir names
        self.files   = files   # list of local file names

        self.name    = os.path.basename(root) # local name of this dir

        # Links to parent and children in the directory tree. These are
        # set as dir objects get mapped within a DirTree instance.

        self.pdo = None
        self.subdos = []

        # Properties from presence of local files, better cached to prevent
        # repeated real file presence checks

        self.req  = self.has_reqtxt()
        self.test = self.has_testpy()
        self.tc   = self.has_tctxt()
        self.set  = self.has_settxt()

        # title name

        self.tname = to_title (self.name)

    def has_reqtxt(self):
        return "req.txt" in self.files

    def has_settxt(self):
        return "set.txt" in self.files

    def has_tctxt(self):
        return "tc.txt" in self.files

    def has_testpy(self):
        return "test.py" in self.files

    # ------------------------------------------
    # -- Bottom-Up node attribute computation --
    # ------------------------------------------

    def botmup_compute_attributes (self, pathi, data):

        # Compute predicates over our set of children. We expect the children
        # attributes to be available here.

        some_tcorset    = False
        some_nottcorset = False

        some_reqorset    = False
        some_notreqorset = False

        some_req      = False
        some_notreq   = False
        some_tc       = False
        some_nottc    = False
        some_set      = False
        some_notset   = False
        some_nottcset = False

        for subdo in self.subdos:
            some_req    |= subdo.req
            some_tc     |= subdo.tc
            some_set    |= subdo.set

            some_notreq   |= not subdo.req
            some_nottc    |= not subdo.tc
            some_notset   |= not subdo.set
            some_nottcset |= not subdo.tcset

            some_tcorset  |= subdo.tc | subdo.tcset
            some_reqorset |= subdo.req | subdo.reqset

            some_nottcorset  |= not (subdo.tc | subdo.tcset)
            some_notreqorset |= not (subdo.req | subdo.reqset)

        self.all_tc    = not some_nottc
        self.all_req   = not some_notreq
        self.all_set   = not some_notset
        self.all_tcset = not some_nottcset

        self.all_reqorset = not some_notreqorset
        self.all_tcorset  = not some_nottcorset

        # For TC sets, consider the difference in consistency between
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

        # We go for 2, to allow subgroups specific to "pragma" contexts (with
        # dedicated sets of drivers) in Topologies sections.

        self.tcset  = self.set and self.all_tc
        self.tctset = self.set and self.all_tcorset

        self.reqset  = self.set and self.all_req
        self.reqtset = self.set and self.all_reqorset

        self.container = self.set or self.req

    def dfile(self, path=False):
        base = (
            'tc.txt' if self.tc else
            'req.txt' if self.req else
            'set.txt' if self.set else
            None)

        return (
            os.path.join (self.root, base) if base and path
            else base)

    def dtext (self):
        if self.dfile() == None:
            warn ("missing description file in %s" % self.root)
            return ""
        else:
            return get_content (self.dfile(path=True))

    def kind (self):
        if self.tc:
            return dcl.TC
        elif self.tcset:
            return dcl.TCSET
        elif self.req:
            return dcl.REQ
        elif self.reqset:
            return dcl.REQSET
        else:
            return dcl.INTRO

# ********************************
# ** Directory Tree abstraction **
# ********************************

# Helper to manage dirname -> dirobject associations and establish
# parent/children relationships over dir objects, assuming the tree
# is walked top down.

# Values to denote possible ways to walk a tree and possible actions to take
# at every directory node when walking the tree

topdown, botmup = range (2)

dirProcess, dirSkip, dirCutPre, dirCutPost = range (4)
# dirProcess: process this node and walk children
# dirCutPre:  don't process this node and don't walk children
# dirCutPost: process this node and don't walk children
# dirSkip:    skip processing for this node, walk children nevertheless

im = { dirSkip: "dirSkip", dirProcess: "dirProcess",
       dirCutPre: "dirCutPre", dirCutPost: "dirCutPost" }

class DirTree:
    def __init__(self, roots):
        self.roots = roots

    # ------------------------------------------------------------
    # -- Tree walking facilities, once we're past the first doc --
    # -- generation pass, all the parent children links are set --
    # ------------------------------------------------------------

    # Local facilities. Beware that walks are used to compute directory
    # object attributes, so these can't be relied upon.

    class WalkInfo:
        def __init__(self, data, pathi, process, ctl, mode):
            self.process = process
            self.ctl = ctl
            self.mode = mode
            self.pathi = pathi
            self.data = data

    def __enter(self, diro, wi):
        if diro.req: wi.pathi.n_req += 1
        if diro.tc: wi.pathi.n_tc += 1
        return wi.ctl (diro, wi.pathi, wi.data)

    def __exit(self, diro, wi):
        if diro.req: wi.pathi.n_req -= 1
        if diro.tc: wi.pathi.n_tc -= 1

    def __visit (self, diro, wi):
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

    def __default_ctl(self, diro, pathi, widata):
        return dirProcess

    # Exposed facilities.

    def walk(self, mode, process, data=None, ctl=None):

        if ctl is None: ctl = self.__default_ctl

        [self.__visit (
                diro=diro, wi=DirTree.WalkInfo (
                    process=process, pathi=PathInfo(),
                    data=data, mode=mode, ctl=ctl))
         for diro in self.roots]

    # ---------------------------------------
    # -- Tree filtering/pruning facilities --
    # ---------------------------------------

    def rprune (self, namelist):

        """prune all subdirs of self that are do *not* lead to any dir
           in NAMELIST"""

        class WalkInfo:
            def __init__(self, tokeep):
                self.tokeep = tokeep
                self.toprune = []

        def prunectl (diro, pathi, wi):

            # See if diro is within or beyond any of our dirs to keep.
            # Keep going (skip) if within. Cut search if beyond.

            # Append "/" to items to prevent "bla/C" from being considered
            # a directory prefix of "bla/Common"

            rootdir = diro.root + "/"

            for tokeep in wi.tokeep:
                dirtokeep = tokeep + "/"
                if os.path.commonprefix ((rootdir, dirtokeep)) == rootdir:
                    return dirSkip if len(rootdir) < len(dirtokeep) else dirCutPost

            # diro is for sure not within or down any of our dirs to keep.
            # register for actual pruning and don't search past it.

            wi.toprune.append (diro)
            return dirCutPost

        # Walk this tree to compute the list of directories to prune, then do
        # prune for real.  We don't effectively prune nodes during the walk to
        # prevent mishaps caused by changes to our internal datastructures
        # while they are being processed.

        wi = WalkInfo (tokeep=namelist)
        self.walk (
            mode=topdown, process=lambda diro, pathi, wi: None,
            ctl=prunectl, data=wi)

        [diro.pdo.subdos.remove (diro) for diro in wi.toprune]

    # ---------------------
    # -- Sorting entries --
    # ---------------------

    def sort (self):

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

            kfile = os.path.join (diro.root, "keys.txt")
            keys = (
                json.loads (get_content(kfile)) if os.path.exists(kfile)
                else {"default": 0}
                )

            # Then sort according to our three criteria of interest

            diro.subdos.sort (
               key = lambda subdo:
                   (subdo.container,
                    keys[subdo.name] if subdo.name in keys
                    else keys["default"],
                    subdo.name)
                )

        self.walk (mode=botmup, process=sort_subdos)

    # -----------------------------------------
    # -- Checking directory tree consistency --
    # -----------------------------------------

    def check_local_consistency (self, diro, pathi):
        """Perform checks on the files present in DIRO"""

        warn_if (not (diro.req or diro.tc or diro.set),
            "missing description text at %s" % diro.root)
        warn_if (diro.req and len(diro.files) > 1,
            "req.txt not alone in %s" % diro.root)
        warn_if (diro.set and len(diro.files) > 1,
            "set.txt not alone in %s" % diro.root)
        warn_if (diro.tc and not diro.test,
            "tc.txt without test.py in %s" % diro.root)
        warn_if (not diro.tc and diro.test,
            "test.py without tc.txt in %s" % diro.root)

        warn_if(diro.files and
                not diro.tc and not diro.set and not diro.req,
            "unexpected files in %s (%s)" % (diro.root, str(diro.files)))

        warn_if ((diro.tc or diro.tcset) and pathi.n_req < 1,
            "tc or set without req uptree at %s" % diro.root)

        warn_if (diro.req and pathi.n_req > 1,
            "nested req (%d up) at %s" % (pathi.n_req - 1, diro.root))

        warn_if (diro.req and pathi.n_tc > 0,
            "req with tc uptree at %s" % diro.root)

    def check_downtree_consistency (self, diro, pathi):
        """Perform checks on the relationships DIRO and its children"""

        warn_if ((diro.req or diro.set) and not diro.subdos,
            "missing subdirs for artifact at %s" % diro.root)

        if not diro.subdos: return

        # Warn on structural inconsistencies

        warn_if (diro.set and not (diro.all_tcorset or diro.all_reqorset),
            "inconsistent subdirs for set.txt at %s" % diro.root)

        warn_if (diro.req and not diro.all_tcorset,
            "inconsistent subdirs down req.txt at %s" % diro.root)

        warn_if (diro.req and not diro.all_reqorset and not diro.all_tcorset,
            "missing testcases for leaf req in %s" % diro.root)

        # Warn on missing testing strategy in leaf requirement with multiple
        # testcases

        warn_if (diro.req and len (diro.subdos) > 1 and diro.all_tcorset
                 and "%(tstrategy-headline)s" not in diro.dtext(),
             "req at %s misses testing strategy description" % diro.root)

        # If one subdo is req, check others

        nreq = 0
        for subdo in diro.subdos:
            if subdo.req: nreq += 1

        warn_if (nreq > 0 and nreq != len (diro.subdos),
            "some but not all are reqs downtree at %s" %  diro.root)

    def topdown_check_consistency (self, diro, pathi, data):
        self.check_local_consistency(diro, pathi)
        self.check_downtree_consistency(diro, pathi)

    def check_consistency(self):
        self.walk (mode=topdown, process=self.topdown_check_consistency)

class DirTree_FromPath (DirTree):

    def __init__(self, rootp):

        DirTree.__init__(self, roots=[])

        # First compute the tree of directory objects starting at path ROOTP,
        # setting up for each the .pdo link to parent and the .subdos list of
        # children. This is achieved by performing a top down walk of the os
        # directory structure.

        self.dir = {}   # dir-name -> dir-object dictionary
        [self.topdown_map (dirname, subdirs, files)
         for (dirname, subdirs, files) in os.walk(os.path.abspath(rootp))]

        # Arrange for the intermediate directories just there for file
        # organisation purposes to disappear from our tree representation

        self.do_cross_overs()

        # Then compute exta node attributes, once the tree of internal
        # directory objects is setup.

        self.compute_attributes()

    def topdown_map(self, dirname, subdirs, files):

        # Map directory DIRNAME into our dictionary and set up links to/from
        # its parent directory, if any. We're called along a topdown walk, so
        # we have mapped the parent directory already if there is one.

        # Ignore some subdirectories

        [subdirs.remove(d) for d in copy(subdirs)
         if d in ('.svn', 'src') or d.startswith('tmp_')]

        # Map a new object for this dir ...

        diro = Dir (root=dirname, subdirs=subdirs, files=files)

        self.dir[dirname] = diro

        # Find out its parent object by name. If there's no parent object,
        # this dir is a root. Setup the links otherwise.

        parentname = os.path.dirname(dirname)

        if parentname not in self.dir:
            self.roots.append(diro)
        else:
            parento = self.dir[parentname]
            diro.pdo = parento
            parento.subdos.append(diro)

    # --------------------------------------------------------------
    # -- Abstract away nodes that were introduced only for source --
    # -- sharing purposes, and that should not introduce an extra --
    # -- layer in the qualification material.                     --
    # -- -----------------------------------------------------------

    def __do_bridge_over(self, diro):

        print "======== Bridging over " + diro.root

        diro.pdo.subdos.remove(diro)

        for subdo in diro.subdos:
            diro.pdo.subdos.append(subdo)
            subdo.pdo = diro.pdo

            subdo.tname += ".%s" % diro.tname

    def __decide_cross_over (self, diro, pathi, wi):

        if diro.set and os.path.getsize (diro.dfile(path=True)) == 0:
            wi.tobridge.append (diro)

    def do_cross_overs(self):

        class WalkInfo:
            def __init__(self):
                self.tobridge = []

        wi = WalkInfo ()
        self.walk (
            mode=topdown, process=self.__decide_cross_over, data=wi)

        [self.__do_bridge_over (diro) for diro in wi.tobridge]

    # ---------------------------------------------
    # -- Computing tree/node attributes, before  --
    # -- more sophisticated walks can take place --
    # ---------------------------------------------

    def compute_attributes(self):
        self.walk (mode=botmup, process=Dir.botmup_compute_attributes)

# ************************
# ** Document Generator **
# ************************

class DocGenerator(object):

    def __init__(self, root_dir, doc_dir):
        self.root_dir = os.path.abspath(root_dir)
        self.doc_dir = os.path.abspath(doc_dir)
        self.resource_list = set([])

        # current output file descriptor, while walking the tor/tc tree
        self.ofd = None

    def register_resources(self, rset):
        self.resource_list |= rset

    def docpath_to(self, filename):
        return os.path.join(self.doc_dir, filename)

    def file2docfile(self, filename):
        """Return the associated filename for a given path"""
        docfile = os.path.relpath(filename, self.root_dir)
        # If we are at the root directory then return our documentation
        # entry point.
        if docfile == ".":
            return "index.rst"

        docfile = docfile.replace('/', '_').replace('\\', '_') + ".rst"
        return docfile

    def ref(self, name):
        """Transform string NAME into another string suitable to be used as
        index name"""
        result = os.path.relpath(name, self.root_dir)
        return result.replace('/', '_').replace('\\', '_').replace('.', '_')

    # ---------------------------------------------
    # -- Generating doc contents for a directory --
    # ---------------------------------------------

    def contents_from(self, diro, name):

        """Fetch descriptive text in file NAME for artifact in DIRO, and
           proceed with supported macro-replacements. Append a TC index to TC
           set descriptions that don't have one."""

        # Fetch the descriptive text

        contents = get_content(os.path.join(diro.root, name))

        # Compute the dictionary of substitutions to apply

        SUBST = {
            "toplevel-index": self.toplev_index,
            "tc-index": self.tc_index,
            "subset-index": self.part_index,
            "part-index": self.part_index,
            "req-headline": self.req_headline,
            "tstrategy-headline": self.tstrat_headline,
            "toc": self.toc
            }

        dosubst = dict(
            [(key, SUBST[key](diro))
             for key in SUBST if ("(%s)s" % key) in contents]
            )

        # Compute the extra text to add at the end, depending on
        # the kind of artifact and on what substitutions apply

        extratext = (
            self.tc_index(diro) if (
            diro.tcset and  "tc-index" not in dosubst
            and "subset-index" not in dosubst)
            else ""
        )

        return contents % dosubst + extratext

    def gen_tc_section(self, diro):
        """Generate the TestCase Sources section, and register the
           sources as resources"""

        tco = TestCase (dir=diro.root, dgen=self)

        self.ofd.write(rest.subsection ("Test Sources"))

        self.ofd.write(subsec_header("Functional Code"))
        self.ofd.write(rest.list(
                [':ref:`%s`' % self.ref(d) for d in sorted(tco.fnsources)]))

        self.ofd.write(subsec_header("Program Tests"))
        self.ofd.write(rest.list(
                    [':ref:`%s`' % self.ref(d) for d in tco.drsources]))

        if tco.conspecs:
            self.ofd.write(subsec_header("Consolidation Tests"))
            self.ofd.write(rest.list(
                    [':ref:`%s`' % self.ref(d) for d in tco.conspecs]))

        self.register_resources (
            tco.fnsources | tco.drsources | tco.conspecs)

    def gen_toc_section(self, diro):
        """Generate the Table Of Contents section as needed"""

        tocentries = [
            self.file2docfile(os.path.join(diro.root, sdo.root))
            for sdo in diro.subdos]

        self.ofd.write('\n\n' + rest.toctree(
                itemlist=tocentries, depth=1, attrlist=[":hidden:"]))

    # --------------------------------------
    # -- Tailored directory tree instance --
    # --------------------------------------

    def dirtree(self, chapdirs):

        dirtree = DirTree_FromPath(rootp=self.root_dir)

        dirtree.rprune (
            [os.path.join (self.root_dir, dir) for dir in chapdirs])

        dirtree.check_consistency()
        dirtree.sort ()

        return dirtree

    # ---------------------------
    # -- generate doc chapters --
    # ---------------------------

    ALT_TITLES = {
        "Qualif": "GNATcoverage TORs"
        }

    def __gen_doc_contents (self, diro, pathi, wi):

        self.ofd = open(
            self.docpath_to (self.file2docfile(diro.root)), 'w')

        ttext = (
            self.ALT_TITLES[diro.tname] if diro.tname in self.ALT_TITLES
            else diro.tname)

        txthdl = diro.kind().txthdl
        self.ofd.write(rest.section(
                ttext + (" -- %s" % txthdl if txthdl else "")
                ))

        if diro.dfile():
            self.ofd.write(self.contents_from (diro=diro, name=diro.dfile()))

        if diro.tc:
            self.gen_tc_section(diro)

        self.gen_toc_section(diro)

        self.ofd.close()


    def generate_chapters(self, dirtree):

        dirtree.walk (mode=topdown, process=self.__gen_doc_contents)

    # ---------------------------
    # -- generate general index --
    # ---------------------------

    def __gen_index_contents (self, diro, pathi, wi):

        self.ofd.write (subsec_header (diro.tname))
        self.ofd.write (self.req_index (diro))

    def generate_genindex(self, dirtree):

        self.ofd = open(self.docpath_to ("sumtables.rst"), 'w')

        self.ofd.write ('\n'.join (
                [".. index::",
                 "   single: Summary tables; Requirements",
                 ""]
                ))

        self.ofd.write (self.headline ("General Requirements Index"))

        dirtree.walk (
            mode=topdown, process=self.__gen_index_contents,
            ctl = lambda diro, pathi, wi:
                (dirSkip if pathi.depth == 0 else dirCutPost)
            )

        self.ofd.close()

    # ---------------------------
    # -- generate index tables --
    # ---------------------------

    # Hackish at times. To be refined ...

    class WalkInfo:
        def __init__ (self, rootp, emphctl):
            self.rootp = rootp

            self.contents = []
            self.emphctl = emphctl

    def maybe_add_line_for (self, diro, pathi, wi):

        # Intermediate sets are sometimes introduced for test drivers
        # organization purposes. They have empty descrpition texts and we
        # don't care for an extra line in the index in this case.

        dtext = diro.dtext().strip()

        # Fetch the contents aimed at the Summary column, first paragraph in
        # the description file.

        toblank = re.search (".*?(\n[ \t]*\n)", dtext, re.DOTALL)
        sumtext = (toblank.group(0) if toblank else dtext).replace ('\n', ' ')

        entrytext = diro.tname

        if wi.emphctl:
            sumtext = wi.emphctl(sumtext.strip(), diro, pathi)
            entrytext = wi.emphctl(entrytext.strip(), diro, pathi)

        linktext = ':doc:`%s <%s>`' % (
            diro.kind().image, self.ref(diro.root))

        # Then append the whole entry

        wi.contents.append (
            '   %s|%s|%s\n' % (linktext, entrytext, sumtext))

    def index_table(
        self, rooto, nodectl, emphctl,
        tblhdr, tblctl=(':widths: 2, 20, 70',)
        ):

        dirtree = DirTree (roots=[rooto])

        wi = self.WalkInfo (
            rootp=rooto.root, emphctl=emphctl)

        # Compute the table header, the entries, and the "link"
        # column legend if needed

        text = '\n' + '\n'.join (
            ['.. csv-table::',
             '   :delim: |']
            + ['   :header: %s' % ','.join (tblhdr)]
            + ['   ' + item for item in tblctl]
            ) + "\n\n"

        dirtree.walk (
            mode=topdown, process=self.maybe_add_line_for,
            ctl=nodectl, data=wi)

        text += ''.join (wi.contents)

        linkcolheader = tblhdr[0]
        if linkcolheader:
            text = '\n'.join (
                [text, "",
                 "**%s** " % linkcolheader
                 + "In such tables, this column always features an "
                 + "hyperlink to the section described by the line, "
                 + "with a text indicative of the section kind:",
                 ""] + ["* *%s*\tdesignates some %s" % (k.image, k.txthdl)
                        for k in dcl.kinds]
                )

        return text

    def tc_index(self, diro):
        return self.index_table (
            rooto   = diro,
            tblhdr  = ("", "Testcase or Group", "Description"),
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if diro.container and pathi.depth == 1
                 else rest.emphasis(text) if diro.container
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirProcess if pathi.depth > 0
                 else dirSkip)
            )

    def subset_index(self, diro, tblhdr = None):
        return self.index_table (
            rooto   = diro,
            tblhdr  = tblhdr,
            emphctl = None,
            nodectl = lambda diro, pathi, wi:
                (dirCutPost if pathi.depth > 0 and (diro.set or diro.req)
                 else dirSkip)
            )

    def part_index(self, diro):
        return self.subset_index (diro, tblhdr = ("", "Part", "Description"))

    def toplev_index(self, diro):
        return self.index_table (
            rooto   = diro,
            tblhdr = ("(*)", "Chapter", "Description"),
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if pathi.depth == 1
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirProcess if pathi.depth == 1 and diro.container
                 else dirCutPost if pathi.depth > 1 and diro.container
                 else dirSkip)
            )

    def req_index(self, diro):
        return self.index_table (
            rooto   = diro,
            tblhdr  = ("", "Requirement or Group", "Description"),
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if diro.container and pathi.depth == 0
                 else rest.emphasis(text) if diro.container
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirSkip if pathi.depth == 0
                 else dirCutPre if diro.tc or diro.tcset
                 else dirProcess)
            )

    # ---------------------------------
    # -- req and tstrategy headlines --
    # ---------------------------------

    def headline (self, text):
        return rest.subsection (text)

    def req_headline (self, diro, plural=False):
        return self.headline ("Requirement" + ("s" if plural else ""))

    def tstrat_headline (self, diro):
        return self.headline ("Testing Strategy")

    # -----------------
    # -- toc entries --
    # -----------------

    def toc(self, diro):

        tocentries = [
            self.file2docfile(os.path.join(diro.root, sd))
            for sd in diro.subdirs]

        return ''.join (
            [sec_header("Links"),
             rest.toctree(tocentries, depth = 1 if not diro.pdo else 2)]
            ) if tocentries else ""

    # ----------------------------
    # -- generate resource file --
    # ----------------------------

    def generate_resources(self):
        fd = open(self.docpath_to ('resources.rst'), 'w')
        fd.write(rest.chapter('Resources'))
        fd.write(rest.toctree(
                [self.file2docfile(d) for d in self.resource_list]))
        fd.close()

        for r in self.resource_list:
            fd = open(self.docpath_to(self.file2docfile(r)), 'w')
            fd.write('\n.. _%s:\n\n' % self.ref(r))
            fd.write(rest.section(os.path.basename(r)))
            fd.write(rest.code_block(get_content(r), 'ada'))
            fd.close()

    # ---------------------------------------------
    # -- generate all the document items,        --
    # -- checking tree consistency along the way --
    # ---------------------------------------------

    def generate_all(self, chapdirs):

        ref_chapdirs = [
            "Appendix", "Common", "Ada/stmt", "Ada/decision", "Ada/mcdc"]

        # [Re]generate only the requested chapters, when specified,
        # everything otherwise

        dirtree = self.dirtree (
            ref_chapdirs if chapdirs is None else chapdirs)

        self.generate_chapters(dirtree)
        self.generate_genindex(dirtree)

        self.generate_resources()

# The main of the script
if __name__ == "__main__":
    mygen = DocGenerator(ROOT_DIR, DOC_DIR)

    mygen.generate_all(
        chapdirs = sys.argv[1:] if len (sys.argv) > 1 else None)
