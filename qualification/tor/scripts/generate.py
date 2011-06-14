#!/usr/bin/env python

import os
import rest
import glob
import re
import sys

DOC_DIR = "source"
ROOT_DIR = "../../../testsuite/Qualif"

# **********************
# ** Helper functions **
# **********************

def get_content(filename):
    """Return contents of a file"""
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

def warn_if(cond, text):
    if cond: print "warning: %s" % text


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
        self.depth = 0  # Depth of this node wrt root of walk operation

# ***************************
# ** Directory abstraction **
# ***************************

# DocGenerator helper to process one specific subdirectory of the TOR/TC
# hierarchy

class Dir:
    def __init__(self, root, subdirs, files):

        # Filesystem attributes for this directory

        self.root    = root    # path to this dir
        self.subdirs = subdirs # list of local subdir names
        self.files   = files   # list of local file names

        # Links to parent and children in the directory tree. These are
        # set as dir objects get mapped within a DirTree instance. We expect
        # these to be constructed in a top-down fashion, and the up link is
        # assumed to be setup before other methods are called (so we know if
        # this is a root section)

        self.pdo = None
        self.subdos = []

        # Properties from presence of local files, better cached to prevent
        # repeated real file presence checks

        self.req  = self.has_reqtxt()
        self.test = self.has_testpy()
        self.tc   = self.has_tctxt()
        self.set  = self.has_settxt()

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

        some_req     = False
        some_notreq  = False
        some_tc      = False
        some_nottc   = False
        some_set     = False
        some_notset  = False

        for subdo in self.subdos:
            some_req    |= subdo.req
            some_tc     |= subdo.tc
            some_set    |= subdo.set

            some_notreq  |= not subdo.req
            some_nottc   |= not subdo.tc
            some_notset  |= not subdo.set

            some_tcorset  |= subdo.tc | subdo.tcset
            some_reqorset |= subdo.req | subdo.reqset

            some_nottcorset  |= not (subdo.tc | subdo.tcset)
            some_notreqorset |= not (subdo.req | subdo.reqset)

        self.all_tc = not some_nottc
        self.all_req = not some_notreq
        self.all_set = not some_notset

        self.all_reqorset = not some_notreqorset
        self.all_tcorset  = not some_nottcorset

        self.tcset = self.set and self.all_tc
        self.reqset = self.set and self.all_req

        self.container = self.set or self.req

    def dfile(self):
        return ('tc.txt' if self.tc else
                'req.txt' if self.req else
                'set.txt' if self.set else
                None)

    def dtext (self):
        return get_content (os.path.join (self.root, self.dfile()))

# ********************************
# ** Directory Tree abstraction **
# ********************************

# Helper to manage dirname -> dirobject associations and establish
# parent/children relationships over dir objects, assuming the tree
# is walked top down.

# Values to denote possible ways to walk a tree and possible actions to take
# at every directory node when walking the tree

topdown, botmup = range (2)

dirProcess, dirSkip, dirCut = range (3)
# dirProcess: process this node and walk children
# dirCut:     process this node and don't walk children
# dirSkip:    skip processing for this node, walk children nevertheless

im = { dirSkip: "dirSkip", dirProcess: "dirProcess", dirCut: "dirCut" }

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
        return wi.ctl (diro, wi.pathi, wi.data)

    def __exit(self, diro, wi):
        if diro.req: wi.pathi.n_req -= 1

    def __visit (self, diro, wi):
        ctl = self.__enter(diro, wi)

        if ctl != dirSkip and wi.mode == topdown:
            wi.process(diro, wi.pathi, wi.data)

        if ctl != dirCut:
            wi.pathi.depth += 1
            [self.__visit(subdo, wi) for subdo in diro.subdos]
            wi.pathi.depth -= 1

        if ctl != dirSkip and wi.mode == botmup:
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

            # See if diro is within or beyond any of our dirs to keep
            # Keep going (skip) if within. Cut search if beyond.

            for tokeep in wi.tokeep:
                if os.path.commonprefix ((diro.root, tokeep)) == diro.root:
                    return dirSkip if len(diro.root) < len(tokeep) else dirCut

            # diro is for sure not within or down any of our dirs to keep.
            # register for actual pruning and don't search past it.

            wi.toprune.append (diro)
            return dirCut

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

        self.walk (
            mode=botmup,
            process=lambda diro, pathi, wi: (diro.subdos.sort (
                    key = lambda subdo:
                        (subdo.container, subdo.root)
                    ))
            )

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

    def check_downtree_consistency (self, diro, pathi):
        """Perform checks on the relationships DIRO and its children"""

        warn_if ((diro.req or diro.set) and not diro.subdos,
            "missing subdirs for artifact at %s" % diro.root)

        if not diro.subdos: return

        # Warn on structural inconsistencies

        warn_if (diro.set and not (diro.all_tcorset or diro.all_reqorset),
            "inconsistent subdirs for set.txt at %s" % diro.root)

        warn_if (diro.req and not (diro.all_reqorset or diro.all_tcorset),
            "inconsistent subdirs down req.txt at %s" % diro.root)

        warn_if (diro.req and not diro.all_reqorset and not diro.all_tcorset,
            "missing testcases for leaf req in %s" % diro.root)

        # Warn on missing testing strategy in leaf requirement with multiple
        # testcases

        warn_if (diro.req and len (diro.subdos) > 1 and diro.all_tcorset
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

        SUBST = {
            "toplevel-index": self.toplev_index,
            "tc-index": self.tc_index,
            "subset-index": self.subset_index,
            "reqs-headline": self.reqs_headline,
            "req-headline": self.req_headline,
            "tstrategy-headline": self.tstrat_headline,
            "toc": self.toc
            }

        contents = get_content(os.path.join(diro.root, name))
        return contents % dict(
            [(key, SUBST[key](diro))
             for key in SUBST if ("(%s)s" % key) in contents]
            )

    def gen_tc_section(self, diro):
        """Generate the TestCase description section"""

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

    ALT_TITLES = {
        "Qualif": "GNATcoverage TORs"
        }

    def gen_doc_contents (self, diro, pathi, wi):
        dest_filename = self.file2docfile(diro.root)
        self.ofd = open(os.path.join(self.doc_dir, dest_filename), 'w')

        ttext = os.path.basename(diro.root)
        ttext = self.ALT_TITLES[ttext] if ttext in self.ALT_TITLES else ttext

        self.ofd.write(rest.section(to_title(ttext)))

        if diro.dfile():
            self.ofd.write(self.contents_from (diro=diro, name=diro.dfile()))

        if diro.tc:
            self.gen_tc_section(diro)

        self.ofd.close()

    # ---------------------------
    # -- generate doc chapters --
    # ---------------------------

    def generate_chapters(self, chapdirs):

        dirtree = DirTree_FromPath(rootp=self.root_dir)

        dirtree.rprune (
            [os.path.join (self.root_dir, dir) for dir in chapdirs])

        dirtree.check_consistency()

        dirtree.sort ()
        dirtree.walk (mode=topdown, process=self.gen_doc_contents)

    # ---------------------------
    # -- generate index tables --
    # ---------------------------

    # We generate simple sphinx tables like
    #
    # ====== =======
    # TC dir Summary
    # ====== =======
    # .../If <first sentence in tc.txt>
    # ====== =======

    # To be refined ...

    class WalkInfo:
        def __init__ (self, rootp, emphctl):
            self.max_tclen = 0
            self.rootp = rootp

            self.contents = []
            self.emphctl = emphctl

    def tc_text(self, diro):
        return ':doc:`%s`' % self.ref(diro.root)

    def maybe_add_line_for (self, diro, pathi, wi):

        # Intermediate sets are sometimes introduced for test drivers
        # organization purposes. They have empty descrpition texts and we
        # don't care for an extra line in the index in this case.

        dtext = diro.dtext().strip()

        if not dtext:
            return

        # Fetch the contents aimed at the Summary column, first paragraph in
        # the description file.

        toblank = re.search (".*?(\n[ \t]*\n)", dtext, re.DOTALL)
        sumtext = (toblank.group(0) if toblank else dtext).replace ('\n', ' ')

        if wi.emphctl:
            sumtext = wi.emphctl(sumtext.strip(), diro, pathi)

        # Then append the whole entry

        wi.contents.append ('%-*s %s\n' % (
                wi.max_tclen, self.tc_text(diro=diro),
                sumtext))

    def compute_max_tclen(self, diro, pathi, wi):
        thislen = len (self.tc_text(diro=diro))
        if thislen > wi.max_tclen:
            wi.max_tclen = thislen

    def index_table(self, rooto, nodectl, emphctl):

        dirtree = DirTree (roots=[rooto])

        wi = self.WalkInfo (rootp=rooto.root, emphctl=emphctl)

        # We first need to compute the common length for all the items
        # in the first column

        dirtree.walk (
            mode=topdown, process=self.compute_max_tclen,
            ctl=nodectl, data=wi)

        # Then we compute the table header, the entries, and the table footer

        text = ''.join ([
            "%-s ========\n" % ('=' * wi.max_tclen),
            "%-*s Summary\n" % (wi.max_tclen, "Entry"),
            "%-s ========\n" % ('=' * wi.max_tclen)
            ])

        dirtree.walk (
            mode=topdown, process=self.maybe_add_line_for,
            ctl=nodectl, data=wi)

        text += ''.join (wi.contents)

        text += "%-s ========\n" % ('=' * wi.max_tclen)

        return text

    def tc_index(self, diro):
        return self.index_table (
            rooto   = diro,
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if diro.container and pathi.depth == 1
                 else rest.emphasis(text) if diro.container
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirProcess if pathi.depth > 0
                 else dirSkip)
            )

    def subset_index(self, diro):
        return self.index_table (
            rooto   = diro,
            emphctl = None,
            nodectl = lambda diro, pathi, wi:
                (dirCut if pathi.depth > 0 and (diro.set or diro.req)
                 else dirSkip)
            )

    def toplev_index(self, diro):
        return self.index_table (
            rooto   = diro,
            emphctl = lambda text, diro, pathi:
                (rest.strong(text) if pathi.depth == 1
                 else text),
            nodectl = lambda diro, pathi, wi:
                (dirProcess if pathi.depth == 1 and diro.container
                 else dirCut if pathi.depth > 1 and diro.container
                 else dirSkip)
            )

    # ---------------------------------
    # -- req and tstrategy headlines --
    # ---------------------------------

    def req_headline (self, diro, plural=False):
        return rest.subsection ("Requirement" + ("s" if plural else ""))

    def reqs_headline (self, diro):
        return self.req_headline (diro, plural=True)

    def tstrat_headline (self, diro):
        return rest.subsection ("Testing Strategy")

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
        fd = open(os.path.join(self.doc_dir, 'resources.rst'), 'w')
        fd.write(rest.chapter('Resources'))
        fd.write(rest.toctree(
                [self.file2docfile(d) for d in self.resource_list]))
        fd.close()

        for r in self.resource_list:
            fd = open(os.path.join(self.doc_dir, self.file2docfile(r)), 'w')
            fd.write('\n.. _%s:\n\n' % self.ref(r))
            fd.write(rest.section(os.path.basename(r)))
            fd.write(rest.code_block(get_content(r), 'ada'))
            fd.close()

    def generate_toc(self):
        fd = open(os.path.join(self.doc_dir, 'toc.rst'), 'w')
        fd.write(
            '\n'.join ([".. toctree::", "   :glob:", '', "   *"]) + '\n\n'
            )
        fd.close()


    # ---------------------------------------------
    # -- generate all the document items,        --
    # -- checking tree consistency along the way --
    # ---------------------------------------------

    def generate_all(self, chapdirs):

        ref_chapdirs = [
            "0_Common", "Ada/stmt", "Ada/decision", "Ada/mcdc"]

        # [Re]generate only the requested chapters, when specified,
        # everything otherwise

        self.generate_chapters(
            ref_chapdirs if chapdirs is None else chapdirs)

        self.generate_resources()
        self.generate_toc()

# The main of the script
if __name__ == "__main__":
    mygen = DocGenerator(ROOT_DIR, DOC_DIR)

    mygen.generate_all(
        chapdirs = sys.argv[1:] if len (sys.argv) > 1 else None)
