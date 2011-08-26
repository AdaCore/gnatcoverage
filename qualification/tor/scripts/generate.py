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

# **************************
# ** Node Set abstraction **
# **************************

# Used to compute common attributes (such as all_req etc) on a set of nodes

class NodeSet:
    def __init__(self, diros=()):

        self.some_reqgroup    = False
        self.some_notreqgroup = False
        self.some_tcgroup     = False
        self.some_nottcgroup  = False
        self.some_appmat    = False
        self.some_notappmat = False

        self.some_reqorset    = False
        self.some_notreqorset = False
        self.some_tcorset     = False
        self.some_nottcorset  = False

        self.some_reqorgroup    = False
        self.some_notreqorgroup = False
        self.some_tcorgroup     = False
        self.some_nottcorgroup  = False

        self.some_req       = False
        self.some_notreq    = False
        self.some_tc        = False
        self.some_nottc     = False
        self.some_app       = False
        self.some_notapp    = False
        self.some_set       = False
        self.some_notset    = False

        self.some_notreqset = False
        self.some_nottcset  = False
        self.some_notappset = False

        self.diros = []

        [self.register_one (diro) for diro in diros]
        self.sync()

    def register_one (self, diro):
        self.some_req    |= diro.req
        self.some_tc     |= diro.tc
        self.some_set    |= diro.set
        self.some_app    |= diro.app

        self.some_notreq    |= not diro.req
        self.some_nottc     |= not diro.tc
        self.some_notset    |= not diro.set
        self.some_notapp    |= not diro.app

        self.some_notreqset |= not diro.reqset
        self.some_nottcset  |= not diro.tcset
        self.some_notappset |= not diro.appset

        self.some_reqgroup    |= diro.reqgroup
        self.some_notreqgroup |= not diro.reqgroup

        self.some_tcgroup     |= diro.tcgroup
        self.some_nottcgroup  |= not diro.tcgroup

        self.some_appmat     |= diro.appmat
        self.some_notappmat  |= not diro.appmat

        self.some_reqorset |= diro.req | diro.reqset
        self.some_tcorset  |= diro.tc | diro.tcset

        self.some_notreqorset |= not (diro.req | diro.reqset)
        self.some_nottcorset  |= not (diro.tc | diro.tcset)

        self.some_reqorgroup |= diro.req | diro.reqgroup
        self.some_tcorgroup  |= diro.tc | diro.tcgroup

        self.some_notreqorgroup |= not (diro.req | diro.reqgroup)
        self.some_nottcorgroup  |= not (diro.tc | diro.tcgroup)

        self.diros.append (diro)

    def sync(self):

        # Beware of queries over empty sets. All the some_ attributes
        # start False, so the all_ attributes would turn true if we're
        # not cautious

        has_diro = len(self.diros) > 0

        self.all_tc     = has_diro and not self.some_nottc
        self.all_req    = has_diro and not self.some_notreq
        self.all_set    = has_diro and not self.some_notset
        self.all_app    = has_diro and not self.some_notapp

        self.all_reqset = has_diro and not self.some_notreqset
        self.all_tcset  = has_diro and not self.some_nottcset
        self.all_appset = has_diro and not self.some_notappset

        self.all_reqorset = has_diro and not self.some_notreqorset
        self.all_tcorset  = has_diro and not self.some_nottcorset

        self.all_reqgroup = has_diro and not self.some_notreqgroup
        self.all_tcgroup  = has_diro and not self.some_nottcgroup
        self.all_appmat   = has_diro and not self.some_notappmat

        self.all_reqorgroup = has_diro and not self.some_notreqorgroup
        self.all_tcorgroup  = has_diro and not self.some_nottcorgroup

# ***************************
# ** Directory abstraction **
# ***************************

# The kind of material node that the directory holds. This is used
# to determine the hyperlink and the page title texts

class DirKind:
    def __init__ (self, txthdl, image):
        self.image  = image
        self.txthdl = txthdl

class dcl:

    TC = DirKind (
        image="tc", txthdl = "Testcase")

    TCG = DirKind (
        image="tcg", txthdl = "Testcase Group")

    REQ = DirKind (
        image="rq", txthdl = "Requirement")

    REQG = DirKind (
        image="rqg", txthdl = "Requirement Group")

    APP = DirKind (
        image="app", txthdl = "Appendix Material")

    INTRO = DirKind (
        image="intro", txthdl = "Introductory Material")

    kinds = (TC, TCG, REQ, REQG, APP, INTRO)

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

        # NodeSet for subdos, which we'll populate when computing
        # attributes bottom-up

        self.sdset = None

        # Properties from presence of local files, better cached to prevent
        # repeated real file presence checks

        self.req  = "req.txt" in self.files
        self.hreq = "hreq.txt" in self.files
        self.test = "test.py" in self.files
        self.tc   = "tc.txt" in self.files
        self.htc  = "htc.txt" in self.files
        self.set  = "set.txt" in self.files
        self.app  = "app.txt" in self.files

        # title name, to be displayed in index tables, and sorting key.

        # The sorting key is the directory name (not title name) for starters,
        # which is meant to allow prefix characters (numbers prior to the
        # first '_') to influence sorts while not being displayed. We will
        # need to update that when bridging over nodes, to make sure that the
        # children remain grouped wrt their new parent and set of siblings.

        self.tname = to_title (self.name)
        self.sname = self.name

    # ------------------------------------------
    # -- Bottom-Up node attribute computation --
    # ------------------------------------------

    def botmup_compute_attributes (self, pathi, data):

        # Compute predicates over our set of children. We expect the children
        # attributes to be available here.

        self.sdset = NodeSet (diros=self.subdos)

        # Populate extra attributes on this node

        self.container = self.set or self.req

        # TC, APP or REQ SETS are nodes with consistent children (all reqs,
        # all tcs, or all apps).

        self.tcset  = self.set and self.sdset.all_tc
        self.reqset  = self.set and self.sdset.all_req
        self.appset  = self.set and self.sdset.all_app

        # Some mixes are legitimate, as long as the leaf item kinds (tc
        # or req) remain consistent. We call GROUPS those containers, and at
        # this stage we let the notion propagate up pretty freely:

        self.tcgroup = self.set and self.sdset.all_tcorgroup
        self.reqgroup = self.set and self.sdset.all_reqorgroup
        self.appmat = self.app or (self.set and self.sdset.all_appmat)

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
            dcl.TC if self.tc
            else dcl.REQ if self.req
            else dcl.TCG if self.tcgroup
            else dcl.REQG if self.reqgroup
            else dcl.APP if self.appmat
            else dcl.INTRO)

    def dfile(self, path=False):
        base = (
            'tc.txt'   if self.tc else
            'set.txt'  if self.set else
            'req.txt'  if self.req else
            'htc.txt'  if self.htc else
            'hreq.txt' if self.hreq else
            'app.txt'  if self.app else
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

            rootdir = diro.root + os.sep

            for tokeep in wi.tokeep:
                dirtokeep = tokeep + os.sep
                if os.path.commonprefix ((rootdir, dirtokeep)) == rootdir:
                    return (
                        dirSkip if len(rootdir) < len(dirtokeep)
                        else dirCutPre)

            # diro is for sure not within or down any of our dirs to keep.
            # register for actual pruning and don't search past it.

            wi.toprune.append (diro)
            return dirCutPre

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
                    keys[subdo.sname] if subdo.sname in keys
                    else keys["default"],
                    subdo.sname)
                )

        self.walk (mode=botmup, process=sort_subdos)

    # -----------------------------------------
    # -- Checking directory tree consistency --
    # -----------------------------------------

    def check_local_consistency (self, diro, pathi):
        """Perform checks on the files present in DIRO"""

        warn_if (
            not (diro.set or diro.req or diro.tc or diro.app
                 or diro.htc or diro.hreq),
            "missing description text at %s" % diro.root)
        warn_if (diro.req and len(diro.files) > 1,
            "req.txt not alone in %s" % diro.root)
        warn_if (diro.set and len(diro.files) > 1,
            "set.txt not alone in %s" % diro.root)
        warn_if (diro.tc and not diro.test,
            "tc.txt without test.py in %s" % diro.root)
        warn_if (diro.test and not diro.tc and not diro.htc,
            "test.py without tc.txt in %s" % diro.root)

        warn_if ((diro.tc or diro.tcgroup) and pathi.n_req < 1,
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

        warn_if (diro.req and not diro.sdset.all_tcorgroup,
            "inconsistent subdirs down req.txt at %s" % diro.root)

        warn_if (
            diro.req and not (diro.sdset.all_reqorgroup
                              or diro.sdset.all_tcorgroup),
            "missing testcases for leaf req in %s" % diro.root)

        # Warn on missing testing strategy in leaf requirement with multiple
        # testcases

        warn_if (diro.req and len (diro.subdos) > 1 and diro.sdset.all_tcorset
                 and "%(tstrategy-headline)s" not in diro.dtext(),
             "req at %s misses testing strategy description" % diro.root)

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

            subdo.tname = '.'.join ([diro.tname, subdo.tname])
            subdo.sname = '.'.join ([diro.sname, subdo.sname])

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

icLink, icNid, icBrief = range (3)

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
            "app-index": self.app_index,
            "subset-index": self.subset_index,
            "global-reqindex": self.global_reqindex,
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
            diro.tcgroup and  "tc-index" not in dosubst
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

    def root_dirtree(self, chapdirs):

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

        txthdl = diro.kind.txthdl
        self.ofd.write(rest.section(
                ttext + (" -- %s" % txthdl if txthdl else "")
                ))

        if diro.dfile():
            self.ofd.write(self.contents_from (diro=diro, name=diro.dfile()))

        if diro.tc:
            self.gen_tc_section(diro)

        self.gen_toc_section(diro)

        self.ofd.close()


    def generate_chapters(self):

        self.dirtree.walk (mode=topdown, process=self.__gen_doc_contents)

    # ---------------------------
    # -- generate index tables --
    # ---------------------------

    # To be refined ...

    class WalkInfo:
        def __init__ (self, rootp, emphctl):
            self.rootp = rootp
            self.emphctl = emphctl

            self.topdepth = sys.maxint

            self.text = None

            self.lset = NodeSet()

    def __maybe_addline_for (self, diro, pathi, wi):

        if pathi.depth < wi.topdepth:
            wi.topdepth = pathi.depth

        # Fetch the contents aimed at the Summary column, first paragraph in
        # the description file.

        dtext = diro.dtext().strip()
        toblank = re.search (".*?(\n[ \t]*\n)", dtext, re.DOTALL)
        sumtext = (toblank.group(0) if toblank else dtext).replace ('\n', ' ')

        linktext = ':doc:`%s <%s>`' % (
            diro.kind.image, self.ref(diro.root))

        entrytext = "%s%s" % (
            ". . " * (pathi.depth - wi.topdepth), diro.tname)

        if wi.emphctl:
            sumtext = wi.emphctl(sumtext.strip(), diro, pathi)
            entrytext = wi.emphctl(entrytext.strip(), diro, pathi)

        # Then append the whole entry

        wi.text.append (
            '   %s#%s#%s' % (linktext, entrytext, sumtext))

        wi.lset.register_one (diro=diro)

    def index_table(self, rooto, nodectl, emphctl, tblhdr):

        local_dirtree = DirTree (roots=[rooto])

        wi = self.WalkInfo (
            rootp=rooto.root, emphctl=emphctl)

        # Start by computing the list of lines of the table body

        wi.text = []

        local_dirtree.walk (
            mode=topdown, process=self.__maybe_addline_for,
            ctl=nodectl, data=wi)

        # sync the line attributes and pick defaults for each column header

        wi.lset.sync()

        if icLink not in tblhdr:
            tblhdr[icLink] = ""

        if icNid not in tblhdr:
            tblhdr[icNid] = (
                "Testcase" if wi.lset.all_tc
                else "Testcase Group" if wi.lset.all_tcgroup
                else "Testcase or Group" if wi.lset.all_tcorgroup
                else "Requirement" if wi.lset.all_req
                else "Requirement Group" if wi.lset.all_reqgroup
                else "Requirement or Group" if wi.lset.all_reqorgroup
                else "Item")

        if icBrief not in tblhdr:
            tblhdr[icBrief] = "Description"

        # Compute the table header

        wi.text = [
            "",
            '.. csv-table::',
            '   :delim: #',
            '   :widths: 2, 20, 70'
            ] + [
            '   :header: %s' % ','.join (
                    [tblhdr[cid] for cid in [icLink, icNid, icBrief]]
                    )
            ] + ['', ''] + wi.text

        linkcolheader = tblhdr[icLink]
        if linkcolheader:
            wi.text.extend (
                ["",
                 "**%s** " % linkcolheader
                 +
                 "For each section described by a line, this column features "
                 "an hyperlink to the section contents. The hyperlink text is "
                 "indicative of the section kind:",
                 ""] + ["* *%s*\tdesignates some %s" % (k.image, k.txthdl)
                        for k in dcl.kinds]
                )


        wi.text = '\n'.join (wi.text)
        return wi

    def tc_index(self, diro):
        return self.index_table (
            rooto   = diro,
            tblhdr  = {},
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if diro.container and pathi.depth == 1
                 else rest.emphasis(text) if diro.container
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirProcess if pathi.depth > 0
                 else dirSkip)
            ).text

    def subset_index(self, diro):
        return self.index_table (
            rooto   = diro,
            tblhdr  = {},
            emphctl = None,
            nodectl = lambda diro, pathi, wi:
                (dirCutPost if pathi.depth > 0 and (diro.set or diro.req)
                 else dirSkip)
            ).text

    def app_index(self, diro):
        return self.index_table (
            rooto   = diro,
            tblhdr  = {},
            emphctl = None,
            nodectl = lambda diro, pathi, wi:
                (dirProcess if pathi.depth > 0 and diro.appmat
                 else dirSkip)
            ).text

    def toplev_index(self, diro):
        return self.index_table (
            rooto   = diro,
            tblhdr = {icLink: "(*)", icNid: "Chapter"},
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if pathi.depth == 1
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirProcess if pathi.depth == 1 and diro.container
                 else dirCutPost if (
                    pathi.depth > 1 and (diro.container or diro.appmat))
                 else dirSkip)
            ).text

    # ---------------------------
    # -- generate general index --
    # ---------------------------

    def local_reqindex(self, diro):
        return self.index_table (
            rooto  = diro,
            tblhdr = {},
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if pathi.depth == 1
                 else rest.emphasis(text) if pathi.depth > 1
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirSkip if pathi.depth == 0
                 else dirCutPost if diro.req
                 else dirProcess)
            )

    def __gen_index_contents (self, diro, pathi, wi):

        lwi = self.local_reqindex (diro)

        lwi.text = ''.join (
            [self.headline (diro.tname + " Requirements"), lwi.text]
            ) if lwi.lset.some_req  else ""

        wi.text = ''.join ([wi.text, lwi.text])

    def global_reqindex(self, diro):

        class WalkInfo:
            def __init__(self):
                self.text=""

        wi = WalkInfo()

        self.dirtree.walk (
            mode=topdown, process=self.__gen_index_contents,
            data=wi, ctl = lambda diro, pathi, wi:
                (dirSkip if pathi.depth == 0 else dirCutPost)
            )

        return wi.text

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

        ref_chapdirs = ["Appendix", "Common", "Ada"]

        # [Re]generate only the requested chapters, when specified,
        # everything otherwise

        self.dirtree = self.root_dirtree (
            ref_chapdirs if chapdirs is None else chapdirs)

        self.generate_chapters()
        self.generate_resources()

# The main of the script
if __name__ == "__main__":
    mygen = DocGenerator(ROOT_DIR, DOC_DIR)

    mygen.generate_all(
        chapdirs = sys.argv[1:] if len (sys.argv) > 1 else None)
