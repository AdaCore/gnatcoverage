# ****************************************************************************
# *                              VIRTUAL TREES                               *
# ****************************************************************************

# Abstractions to allow the construction of internal representations
# of directory trees, with objects to represent directory nodes and their
# parentship connections.
#
# The most basic abstractions are the Dir and the DirTree class.
#
# Dir instances represent directories. DirTree instances represent Directory
# Trees. (eh :)
#
# Mapping Dir objects into a directory tree during a topdown walk sets
# up references to parent directory objects, which then allows all sorts of
# useful computations.
#
# This file also includes a few other classes to allow the construction
# of virtual directory trees from links to shared filesystem subdirs, e.g.
#
# On the filesystem, from something like:
#
#   lib/chapter1
#      /chapter2
#      ...
#
#   sectionA/
#        link_to_lib/
#   sectionB/
#        link_to_lib/
#
# build a *virtual* directory tree like:
#
#   sectionA/lib/chapter1
#               /chapter2
#
#   sectionB/lib/chapter1
#               /chapter2
#
# This second part is still work in progress.

import json
import os, re
from collections import OrderedDict

from cutils import contents_of
from gnatpython.fileutils import ls

# *************************
# ** DirLink abstraction **
# *************************

class DirLink:

    def __init__(self, vlfile):

        self.file = os.path.abspath(vlfile)
        self.dir = os.path.dirname (self.file)

        keys = json.loads (contents_of(vlfile))

        self.fstarget = self.__resolve (keys["fspath"])

        self.vcat = keys.get ("cat", None)
        self.vlang = keys.get ("lang", None)

    def __resolve(self, fstarget):
        return (
            os.path.join (self.dir, fstarget)
            if not fstarget.startswith (('/', '%'))
            else fstarget)


# ********************************
# ** Directory node abstraction **
# ********************************

class Dir:
    def __init__(self, fspath, subdirs, files):

        # Filesystem attributes for this directory

        self.fspath  = fspath  # path to this dir
        self.subdirs = subdirs # list of local subdir names
        self.files   = files   # list of local file names

        self.name    = os.path.basename(fspath) # local name of this dir

        # Links to parent and children in the virtual tree. These are
        # set as dir objects get mapped within a DirTree instance.

        self.pdo = None
        self.subdos = []

# ***************************
# ** Path Info abstraction **
# ***************************

# Holds info about the path to the current node when walking
# a directory tree

class PathInfo:
    def __init__(self):
        self.depth = 0   # Depth of this node wrt root of walk operation

# ********************************
# ** Directory Tree abstraction **
# ********************************

# Values to denote possible ways to walk a tree and possible actions to take
# at every directory node when walking the tree

topdown, botmup = range (2)

dirProcess, dirSkip, dirCutPre, dirCutPost = range (4)
# dirProcess: process this node and walk children
# dirCutPre:  don't process this node and don't walk children
# dirCutPost: process this node and don't walk children
# dirSkip:    skip processing for this node, walk children nevertheless

class DirTree:

    def __init__(self, rooto=None):

        self.rooto = rooto

        # { fspath -> dir-object }
        self.dirmap = OrderedDict()

        # { dto -> [DirLink objects for link files dto] }
        self.linkmap = {}

    # Local facilities. Beware that walks are used to compute directory
    # object attributes, so these can't be relied upon.

    class _WalkInfo:
        def __init__(self, pathi, ctl, mode, data):
            self.ctl   = ctl
            self.mode  = mode
            self.pathi = pathi
            self.data  = data
            self.nodeq = []

    def _walk_from (self, diro, wi):

        this_ctl = wi.ctl (diro, wi.pathi, wi.data)

        visit_children = this_ctl not in [dirCutPre, dirCutPost]
        process_this = this_ctl not in [dirSkip, dirCutPre]

        if process_this and wi.mode == topdown:
            wi.nodeq.append (diro)

        if visit_children:
            wi.pathi.depth += 1
            [self._gather_from (diro, wi) for diro in diro.subdos]
            wi.pathi.depth -= 1

        if process_this and wi.mode == botmup:
            wi.nodeq.append (diro)

    def __default_ctl(self, diro, pathi, widata):
        return dirProcess

    # Exposed facilities.

    # -----------------
    # -- topdown_map --
    # -----------------

    def topdown_map(self, dirname, subdirs, files):

        # Map directory DIRNAME into our dictionary and set up links to/from
        # its parent directory, if any. We're called along a topdown walk, so
        # we have mapped the parent directory already if there is one.

        # Ignore some subdirectories. This is best done by removing from the
        # input list, so the parent filesystem iteration doesn't walk in.

        to_prune = (
            sd for sd in subdirs
            if re.match (
                pattern = "\.svn|tmp_|st_|dc_|mc_",
                string = sd)
            )
        [subdirs.remove(sd) for sd in to_prune]

        # Map a new object for this dir ...

        dirname = dirname.rstrip('/')
        diro = Dir (fspath=dirname, subdirs=subdirs, files=files)
        self.dirmap[dirname] = diro

        # Find out its parent object by name. If there's no parent object,
        # this dir is the tree root. Setup the links otherwise.

        parentname = os.path.dirname(dirname)

        if parentname not in self.dirmap:
            self.rooto = diro
        else:
            parento = self.dirmap[parentname]
            diro.pdo = parento
            parento.subdos.append(diro)

        links = [
            DirLink(os.path.join(dirname, f))
            for f in files if f.endswith(".dtl")
            ]
        if links:
            self.linkmap[diro] = links

        return diro

    # ----------
    # -- walk --
    # ----------

    def walk(self, mode, process, data=None, ctl=None):

        if ctl is None: ctl = self.__default_ctl

        wi=DirTree._WalkInfo (
            pathi=PathInfo(), mode=mode, ctl=ctl, data=data)

        self._walk_from (diro=self.rooto, wi=wi)

        [process (diro, data) for diro in wi.nodeq]

        return wi.nodeq


# *************************************************
# * Processing directory links - Work in progress *
# *************************************************

# !! BIG CHANTIER HERE !!

# LibExp/AndXY/Ada/Var/IfElse
#                  /Aggregates/IfElse
#
#             /C/Var/IfElse

# Qualif/Ada/decision/Robustness
#                    /Expressions -> /path/to/LibExp (Ada, decision)
#
#           /mcdc/Robustness
#                /Expressions -> /path/to/LibExp (Ada, mcdc)
#                             ^^
#                             vlink, textfile which designates
#                             the root of a tree to map here.
#
#                             maps the LibExp subdir as a subir
#                             from the vlink location:
#
#                               Expressions/LibExp/AndXY/...
#                                           ^       /...
#                                           As if the LibExp root
#                                           had been here

# category enforcement ?
#   testsuite argument passed down ?

# language filtering ?
#   consider matching langsubdir only + bridgeover ?
#   -> Qualif/Ada/mcdc/Expressions/AndXY/IfElse
#                      ^          ^
#                      LibExp     /Ada/
#                      plugged    bridged-over
#                      here       here

# ======================
# == DirTree_frompath ==
# ======================

# Compute the tree of directory objects found at a provided root, witout
# following links.

# Helper for each subtree to be constructed and hooked when following links,
# job of DirTree_withlinks

class DirTree_frompath (DirTree):

    def __init__(self, rootp):
        self.rootp = os.path.abspath(rootp)
        DirTree.__init__(self, rooto=None)

    # fetching Dir objects

    def _fsnodes (self):
        return (
            self.topdown_map (dirname, subdirs, files)
            for (dirname, subdirs, files) in os.walk(self.rootp)
            )

    def dirs(self, dirfilter):

        # If we have a node map at hand, use that.  Perform a filesystem walk
        # otherwise, building the node map along the way.

        return (
            diro for diro in (
                self.dirmap if self.dirmap is not None
                else self._fsnodes()
                )
            if dirfilter (diro))

# =======================
# == DirTree_withlinks ==
# =======================

class DirTree_withlinks (DirTree):

    def __init__(self, rootp):
        self.rootp = os.path.abspath(rootp)
        DirTree.__init__(self, rooto=None)

    def __resolve(self, dl):

        SUBST = {
            "root": self.rootp
            }
        return (
            dl.fstarget % dict(
                [(key, SUBST[key])
                 for key in SUBST if ("(%s)s" % key) in dl.fstarget]
                )
            )

    def dirs (self, dirfilter):

        startlinks = [
            DirLink(r) for r in ls ("%s/*.dtl" % self.rootp)
            ]
        rootq = [
            (subdir, None) for subdir in (
                [dtl.fstarget for dtl in startlinks]
                if startlinks else [self.rootp])
            ]

        while rootq:
            (rootp, parento) = rootq.pop()

            dt = DirTree_frompath(rootp)
            for diro in dt.dirs(dirfilter=dirfilter):
                yield diro

            dt.rooto.pdo = parento

            [rootq.append((self.__resolve(dl), diro))
             for diro in dt.linkmap for dl in dt.linkmap[diro]]


# *************************************
# * experimental feature demonstrator *
# *************************************

if __name__ == '__main__':

    # construct a virtual tree incrementally
    # by requesting all the reachable testcases.

    def process_tc (fspath):
        print fspath

    dt = DirTree_withlinks (rootp='.')
    [process_tc (dir.fspath) for dir in
     dt.dirs(dirfilter=lambda diro: 'test.py' in diro.files)]

    print "================================"

    [process_tc (dir.fspath) for dir in
     dt.dirs(dirfilter=lambda diro: 'test.py' in diro.files)]

