#!python

# *****************************************************************************

# General principles
# ==================
#
# This is a helper script aimed at the production and packaging of the
# GNATcoverage qualification material documents. There are three major
# documents of interest:
#
# * PLANS: the GNATcoverage "Tool Qualification Plan" document, produced
#          from
#
# * TOR  : the GNATcoverage "Tool Operational Requirements" document,
#          tree of requirements and accompanying testcase descriptions
#
# * STR  : the GNATcoverage "Software Test Results" report, summary of
#          a qualification testsuite execution for a specific configuration
#          (this target, these tool versions with such options, producing
#          such and such testsuite results)
#
# All the artifacts required to produce these documents are hosted in a GIT
# repository. This ought to be the AdaCore main "gnatcoverage" repository for
# material to be delivered.
#
# The three documents may be produced in either html or pdf format. Most of
# the time, this is achieved by using Sphinx on generated REST.
#
# Packaging essentially consists in setting up a subdir where each document
# has a dedicated place, building a toplevel index and creating a zip archive.
#
# ---
#
# An execution of this script typically proceeds with the following steps:
#
# 1) set up a git clone of the repo where the artifacts are located
#
# 2) build whatever is requested (plans, str, tor) from those artifacts
#    by default, producing the html or pdf document of interest + stuff
#    we don't care about (e.g. intermediate latex sources for rest->pdf),
#
# 3) move or copy the final documents in an ITEMS subdir, then maybe build
#    an index linking to the set of available items.
#
# Everything takes place in a "root" or "work" directory, specified with
# --root-dir or --work-dir.
#
# --root-dir means "use this location, making sure we're starting from
#            scratch"  where starting from scratch is enforced by checking
#            that the designated dir doesn't exist when we start.
#
# This should be used for the final production of material to be delivered,
# and the artifacts repo should be the main AdaCore repo.
#
# --work-dir means "use this location, as is". The designated dir might exist
#            already, have remnants of previous builds etc.
#
# This is useful in development mode when elaborating this or that document
# parts. Needs to be used with care and a minimum understanding of what the
# script does internally: this script is intended to be no more than a simple
# wrapper around lower level operations for each document; it is not designed
# to guarantee 100% accurate results in all situations where you restart,
# after an arbitrary number of stops at arbitrary points. Remnants of previous
# builds can cause surprising results with sphinx, for example, and restarting
# from a clean root is the simplest option at times.
#
# With --work-dir, the target dir might already have a clone setup. By
# default, a selected git source is re-cloned there. --git-reuse and
# --git-pull provide alternate options.
#
# When cloning in a work dir that doesn't have a clone already, or when
# re-cloning when neither --git-reuse nor --git-pull is requested,
# --git-source lets you state which repo is to be cloned. In absence of an
# explicit source, the main AdaCore git repo for GNATcoverage is selected.
#
# Example development sequence:
# =============================
#
# Setting up a root dir from a clone of a local repo, switching to
# the "dev-str" working branch, building plans in html format for starters:
#
#   python genbundle.py --docformat=html
#     --root-dir=$HOME/my-qmat
#     --git-source=$HOME/gnatcoverage --branch=dev-str
#     --parts=plans
#
# Testing plans regeneration after local commit:
#
#   python genbundle.py --docformat=html
#      --work-dir=$HOME/my-qmat
#      --git-pull --branch=dev-str
#      --parts=plans
#
# Testing STR production, designating a local testsuite-dir where a
# qualified testsuite run has taken place:
#
#   python genbundle.py --docformat=html
#     --work-dir=$HOME/my-qmat
#     --git-pull --branch=dev-str
#     --parts=str
#     --testsuite-dir=$HOME/gnatcoverage/testsuite
#     --dolevel=doB
#
# Fetching remote testsuite results:
# ==================================
#
# This is useful e.g. when a kit has to be produced someplace while the
# testsuite has to run elsewhere.
#
# --testsuite-dir supports "remote access" prefixes like "[login@]hostname:"
# for this purpose.
#
# Example kit production commands:
# ================================
#
# Producing a packaged kit, with a toplevel index and a final .zip archive is
# achieved by not restricting to specific parts, hence by not passing --parts
# at all. A DO level has to be provided in this case:
#
#   python genbundle.py --docformat=html
#     --root-dir=$HOME/my-qmat
#     --branch=<project-branch>
#     --dolevel=doA
#     --testsuite-dir=<...>
#
# *****************************************************************************

from gnatpython.ex import Run
from gnatpython.fileutils import cp, mv, rm, mkdir, ls, find

from datetime import date

import optparse, sys, os.path, shutil, re

# This lets us access modules that the testuite
# code features:
MY_TESTSUITE_DIR=os.path.abspath("../testsuite")
sys.path.append(MY_TESTSUITE_DIR)

from SUITE.qdata import CTXDATA_FILE, treeref_at
from SUITE.cutils import load_from, contents_of, output_of

# =======================================================================
# ==                         MISC UTILITY FUNCTIONS                    ==
# =======================================================================

class Error (Exception):
    def __init__(self):
        pass

def fail_if (p, msg):
    if p:
        print msg
        raise Error

def exit_if (p, msg):
    if p:
        print msg
        sys.exit(1)

def run_list (cmd, dir=None):
    """Execute the provided CMD command-list (command name + arguments),
    temporarily switching to the DIR directory unless None, dumping a .log
    file tracking the command output in the directory where the command
    executes."""

    oriwd = os.getcwd()
    print "from : %s" % oriwd

    if dir:
        print "hopat : %s" % dir
        os.chdir (dir)

    print "run  : %s" % ' '.join(cmd)

    out = os.path.basename(cmd[0])+".log"
    p = Run (cmd, output=out, env=None)
    fail_if (
        p.status != 0, "execution failed\n"
        + "log was:\n" + contents_of(out))

    os.chdir (oriwd)

def run (s, dir=None):
    run_list (s.split(), dir)

def announce (s):
    print "=========== " + s

def remove (path):
    """Delete the file or directory subtree designated by PATH"""

    print "from : %s" % os.getcwd()
    print "remove : %s" % path

    # To prevent big damage if the input PATH argument happens to have been
    # miscomputed, we first attempt to move it locally, then remove the local
    # instance. The absence of computation on this local name makes it a tad
    # safer to manipulate and the attempt to move locally would fail for
    # obviously bogus arguments such as anything leading to a parent of the
    # current dir (e.g. "/", or ...).

    local_name = "./old_stuff_to_be_removed"

    # Note that what we have to remove maybe be a regular filee or an entire
    # directory subtree and that rm("recursive=True") is not guaranteed to
    # work for regular files.

    # Start by removing the current local instance, in case the previous
    # removal failed or was interrupted somehow.

    if os.path.exists (local_name):
        rm (local_name, recursive=os.path.isdir(local_name))

    if os.path.exists (path):
        mv (path, local_name)
        rm (local_name, recursive=os.path.isdir(local_name))

# testsuite dirs may include a [user@]remote-host: prefix

def raccess_in (path):
    components = path.split(':')
    return components[0] if len(components) > 1 else None

def rdir_in (path):
    components = path.split(':')
    return components[1] if len(components) > 1 else None

# current git branch name, used as kit identifier:

def current_gitbranch_at (dirname):
    
    cwd = os.getcwd()    
    os.chdir(dirname)

    this_branch = re.search (
        pattern="\* (?P<branchname>\S*)",
        string=output_of("git branch")
        ).group('branchname')
    
    os.chdir(cwd)

    return this_branch

# =======================================================================
# ==              QUALIF MATERIAL GENERATION HELPER CLASS              ==
# =======================================================================

sphinx_target_for = {
    "html": "html",
    "pdf" : "latexpdf"
    }

# The master GIT repo where our source artifacts reside
GIT_MASTER = "ssh://git.eu.adacore.com/scmrepos/git/gnatcoverage"

# The subdir name for this clone, relative to --root
GIT_CLONE_SUBDIR = "gnatcoverage-git-clone"

class QMAT:

    def itemsdir (self):
        return os.path.join (self.rootdir, "%s" % self.this_docformat.upper())

    def __init__(self, options):

        self.o = options

        self.rootdir =  os.path.abspath (
            options.rootdir if options.rootdir else options.workdir)

        self.repodir = os.path.join (self.rootdir, GIT_CLONE_SUBDIR)

        # A local place where the testsuite tree may be found,
        # possibly after remote syncing from testsuite_dir if that
        # is non-local:

        self.local_testsuite_dir = None

        # To be set prior to build_as_needed():

        self.this_docformat = None

        # Sequence number of doc build pass we are processing:

        self.passno = 0

    # --------------------
    # -- setup_workdir --
    # --------------------

    def setup_workdir (self):

        announce ("setting up working dir at %s" % self.rootdir)

        mkdir (self.rootdir)


    # ----------------
    # -- git_update --
    # ----------------

    def git_update (self):

        # If we're requested to pull/update only, do so

        if self.o.gitpull:
            announce ("updating git clone from origin")

            os.chdir(self.repodir)
            run ("git pull --rebase origin")
            return

        # If we're requested to reuse an existing clone, do so

        if self.o.gitreuse:
            announce ("reusing existing git clone")
            return

        # Otherwise, get a fresh clone.

        os.chdir(self.rootdir)

        gitref = (
            self.o.gitsource if self.o.gitsource
            else GIT_MASTER
            )

        announce ("cloning git repository from %s" % gitref)

        remove (GIT_CLONE_SUBDIR)
        run ("git clone %s %s" % (gitref, GIT_CLONE_SUBDIR))

    # ----------------------
    # -- switch_to_branch --
    # ----------------------

    def switch_to_branch (self):
        announce ("switching to branch '%s'" % self.o.branchname)

        os.chdir(self.repodir)
        run ("git checkout %s" % self.o.branchname)

    # -----------------
    # -- kititem_for --
    # -----------------

    def kititem_for (self, part):
        """The name of the filesystem entity which materializes PART in a kit
        distribution.  This will be a subdirectory name for treeish formats
        a-la html (e.g. TOR, which will contain content.html etc), or a
        specific filename (e.g. PLANS.pdf) for other formats. This is what
        latch_into sets up eventually."""
        
        this_item_is_tree = (self.this_docformat == 'html')

        this_item_suffix = (
            '' if this_item_is_tree else '.%s' % self.this_docformat)

        return "%(part)s%(suffix)s" % {
            "part": part.upper(),
            "suffix": this_item_suffix }

    # ----------------
    # -- latch_into --
    # ----------------

    # Helper for the various build_ methods below, to store the just built
    # sphinx output for a provided PART (str, plans, tor) at a toplevel
    # DIR.

    # html builds are voluminous and tree-ish. Other builds might produce
    # secondary pieces we don't need (e.g. latex sources & stuff) and we
    # only care about the final file at the end.

    # For tree builds, we just rename the whole sphinx build tree as our
    # result. For example:
    #
    # build/html for "plans" gets renamed as <TOPLEVEL>/PLANS, a subdirectory
    # where we expect content.html to be found.

    # For other builds, we just copy the actual file, for example:
    #
    # build/pdf/TOR.pdf gets copied as <DIR>/TOR.pdf

    # Eventually, when all the parts are latched, we have something like :
    #
    #    <TOPLEVEL>/PLANS.pdf
    #              /TOR.pdf
    #              /STR.pdf
    #
    # for the pdf versions, and
    #
    #    <TOPLEVEL>/PLANS/index.html etc
    #              /TOR/index.html etc
    #              /STR/index.html etc
    #
    # for the html versions.

    def __latch_into (self, dir, part, toplevel, copy_from=None):

        this_target_is_tree = (self.this_docformat == 'html')

        # Compute the target dir or file name for our copy:

        this_target = (
            dir if toplevel and this_target_is_tree
            else os.path.join (
                dir, self.kititem_for(part=part))
            )

        # Compute the source dir or file name for our copy:

        # The local or provided source build subdir name, assuming a
        # sphinx setup, with an html or pdf sub-subdir depending on the
        # doc format. For file outputs, assume the builders are setup to
        # produce PART.<docformat>, e.g. TOR.pdf:

        this_build_subdir = os.path.join (
            copy_from if copy_from is not None else "build",
            sphinx_target_for[self.this_docformat]
            )

        this_source = (
            this_build_subdir if this_target_is_tree
            else os.path.join(this_build_subdir,
            part.upper() + ".%s" % self.this_docformat)
            )


        # Delete an old version of latched results that might
        # already be there if we're running with --work-dir.

        remove (this_target)

        # Now proceed with the latch operation per se:

        if not this_target_is_tree:
            cp (this_source, this_target, recursive=False)

        elif copy_from:
            cp (this_source, this_target, recursive=True)

        else:
            mv (this_build_subdir, this_target)

        print "%s %s available in %s %s" % (
            self.this_docformat, part.upper(),
            this_target, "(toplevel)" if toplevel else ""
            )

    # ------------------
    # -- gen_qm_model --
    # ------------------

    def gen_qm_model (self):
        """Generate model.xml that __qm_build will use."""

        announce ("generating qm model")

        os.chdir (
            os.path.join (self.repodir, "qualification", "qm")
            )

        run (sys.executable + \
                 " genmodel.py --dolevel=%s" % self.o.dolevel)

    # ----------------
    # -- __qm_build --
    # ----------------

    def __qm_build (self, part):
        """Build one PART of using the Qualifying Machine."""

        announce ("building %s %s" % (self.this_docformat, part.upper()))

        os.chdir (
            os.path.join (self.repodir, "qualification", "qm")
            )
        run ("qmachine model.xml -l scripts/generate_%s_%s.py" \
                 % (part, self.this_docformat))

        self.__latch_into (
                dir=self.itemsdir(), part=part, toplevel=False)

    # ---------------
    # -- build_tor --
    # ---------------

    def build_tor (self):

        # If we have a local testsuite dir at hand and we haven't done so
        # already, fetch the testsresults that the QM needs to check TOR/TC
        # consistency:

        if self.local_testsuite_dir and self.passno == 1:
            os.chdir (self.local_testsuite_dir)
            [cp (tr, os.path.join (
                        self.repodir, "testsuite",
                        os.path.dirname (tr))
                 )
             for tr in find (root=".", pattern="tc.dump")]

        self.__qm_build (part="tor")

    # ------------------------------
    # -- dump_kit_consistency_log --
    # ------------------------------

    def __dump_tree_consistency_info (self, log, suite_ctxdata):

        log.write ("\n-- TREE CONSISTENCY LOG:\n")

        if not suite_ctxdata:
            log.write (
                "suite context info unavailable, "
                "tree consistency unchecked\n")
            return

        os.chdir (self.repodir)

        local_treeref = treeref_at(self.repodir)
        suite_treeref = suite_ctxdata.treeref

        if local_treeref == suite_treeref:
            log.write (
                "artifact tree ref matches testsuite (%s)\n"  % local_treeref)
            return

        log.write (
            "local_treeref = %s , suite_treeref = %s\n"  % \
                (local_treeref, suite_treeref)
            )

        merge_base = output_of (
            "git merge-base %s %s" % (local_treeref, suite_treeref)
            ).strip(' \n')

        (first, next) = (
            (suite_treeref, local_treeref) if merge_base == suite_treeref
            else (local_treeref, suite_treeref) if merge_base == local_treeref
            else (None, None))

        if first == None:
            log.write (
                "!!! local and suite tree refs aren't sequential !!!\n")
            return
        
        log.write (
            "%s tree is ahead\n" % \
                ('suite' if first == local_treeref else 'local')
            )
        
        gitlog_cmd = (
            "git rev-list --oneline %s ^%s" % (next, first))
        log.write (
            '\n'.join (['', gitlog_cmd, '--', output_of(gitlog_cmd), ''])
            )

    def __dump_version_consistency_info (self, log, suite_ctxdata):

        log.write ("\n-- VERSION CONSISTENCY LOG:\n")

        if not suite_ctxdata:
            log.write (
                "suite context info unavailable, "
                "versions consistency unchecked\n")
            return

        def check_one (tool, actual, expected):

            if not expected:
                log.write (
                    "expected %s version NOT specified\n" % tool)
                return

            note = (
                "matches" if re.search (pattern=expected, string=actual)
                else "doesn't match")
            
            log.write (
                '%(tool)s version "%(actual)s" %(note)s ' \
                    'expectation \"%(expected)s"\n' % {
                        'tool'    : tool,
                        'note'    : note,
                        'expected': expected,
                        'actual'  : actual
                        }
                )            
        #

        check_one (
            tool = "gnatpro",
            actual = suite_ctxdata.gnatpro.version,
            expected = self.o.xgnatpro)

        check_one (
            tool = "gnatcov",
            actual = suite_ctxdata.gnatcov.version,
            expected = self.o.xgnatcov)

    def __dump_tr_consistency_info (self, log):

        log.write ("\n-- TOR/TR CONSISTENCY LOG:\n")

        tor_tr_logfile = os.path.join (
            self.repodir, "qualification", "qm", "missing_tr_log.txt")

        if not os.path.exists (tor_tr_logfile):
            log.write ("QM log NOT available\n")
            return

        log.write (
            "%s TOR/TR consistency log from QM @ %s :\n" % \
                ("FRESH" if self.do_tor() else "OLD", tor_tr_logfile)
            )
        log.write (contents_of (tor_tr_logfile))
        log.write ("--\n")

    def dump_kit_consistency_log (self):
        announce ("dumping consistency log - format agnostic")
        
        logfile = os.path.join(self.rootdir, "consistency.log")
        log = open (logfile, 'w')

        log.write (
            "artifact tree at %s (%s)\n" % (
                self.repodir, treeref_at(self.repodir))
            )
        log.write (
            "designated testsuite tree at %s (%s)\n" % \
                (self.o.testsuite_dir,
                 "REMOTE" if raccess_in(self.o.testsuite_dir)
                 else "local")
            )
        log.write (
            "local testsuite tree at %s\n" % self.local_testsuite_dir)        

        suite_ctxdata = load_from (
            os.path.join (self.local_testsuite_dir, CTXDATA_FILE))
        
        self.__dump_tree_consistency_info (log, suite_ctxdata)
        self.__dump_version_consistency_info (log, suite_ctxdata)
        self.__dump_tr_consistency_info (log)

        log.close()

        print "consistency log available at %s" % logfile

    # ----------------------------
    # -- localize_testsuite_dir --
    # ----------------------------

    def __localize_testsuite_dir (self):
        """If testsuite_dir is remote and we haven't fetched
        a local copy yet, do so. Then memorize the local location for
        future attempts."""

        os.chdir (self.rootdir)

        raccess = raccess_in (self.o.testsuite_dir)
        
        if raccess:
            rdir = rdir_in (self.o.testsuite_dir)
            (login, rhost) = (
                raccess.split('@') if '@' in raccess else (None, raccess)
                )

            self.local_testsuite_dir = "%s_testsuite" % rhost

            if self.o.rekit:
                print "rebuilding kit: NOT syncing %s, reusing %s instead" \
                    % (self.o.testsuite_dir, self.local_testsuite_dir)
            else:
                run ("rsync -arz --delete %s:%s/ %s" \
                         % (raccess, rdir, self.local_testsuite_dir)
                     )
        else:
            self.local_testsuite_dir = self.o.testsuite_dir

        self.local_testsuite_dir = \
            os.path.abspath (self.local_testsuite_dir)

    # ---------------------
    # -- prepare_str_dir --
    # ---------------------

    def __prepare_str_dir (self):
        """Helper for build_str. If we haven't done it already, arrange to
        generate the STR REST for the designated testsuite-dir, possibly
        remote.  Fetch this dir back as needed then and remember where the
        corresponding STR subdir (with REST generated) is located."""
        
        announce ("preparing STR dir @ %s" % self.o.testsuite_dir)

        # Produce REST from the tests results dropped by testsuite run.  If we
        # did not launch one just above, expect results to be available from a
        # previous run, possibly remote:

        raccess = raccess_in (self.o.testsuite_dir)
        rdir = rdir_in (self.o.testsuite_dir)

        mkstr_cmd = (
            "(cd %(dir)s/STR && ./mkrest.sh %(level)s)"
            %  {"dir"   : self.o.testsuite_dir if not rdir else rdir,
                "level" : self.o.dolevel}
            )
        prefix = ["sh", "-c"] if not raccess else ["ssh", raccess]
        run_list (prefix + [mkstr_cmd])

    # ---------------
    # -- build_str --
    # ---------------

    def build_str (self):
        announce ("building %s STR" % self.this_docformat)

        os.chdir (os.path.join (self.local_testsuite_dir, "STR"))

        run ("make %s" % sphinx_target_for[self.this_docformat])

        self.__latch_into (
            dir=self.itemsdir(), part='str', toplevel=False)

    # -----------------
    # -- build_plans --
    # -----------------

    def build_plans (self):
        self.__qm_build (part="plans")

    # ---------------
    # -- build_kit --
    # ---------------

    def __relocate_into(self, dir, part):

        the_item = self.kititem_for(part=part)

        item_source_path = os.path.join (self.itemsdir(), the_item)
        item_target_path = os.path.join (dir, the_item)

        remove (item_target_path)

        print "move : %s" % item_source_path
        print "into : %s" % dir
        
        mv (item_source_path, dir)

    def build_kit (self):
        announce ("building %s kit" % self.this_docformat)

        os.chdir (self.rootdir)

        # The kit name is computed as:
        #
        #    gnatcov-qualkit-<kitid>-<YYYYMMDD>
        #
        # where <YYYYMMDD> is the kit production stamp (now), and <kitid>
        # is the git branch from which the artifacts are taken. The git branch
        # name might contain the "qualkit" indication already.

        today = date.today()
        gitbranch = current_gitbranch_at(self.repodir)

        kitprefix = (
            "gnatcov-qualkit" if "qualkit" not in gitbranch
            else "gnatcov"
            )
        kitid = gitbranch

        # If we are re-constructing a kit with some parts just rebuilt, target
        # the specified version (stamp) and arrange to keep the old elements
        # in place:

        kitstamp = (
            self.o.rekit if self.o.rekit
            else "%4d%02d%02d" % (today.year, today.month, today.day)
            )
        kitname = "%s-%s-%s" % (kitprefix, kitid, kitstamp)             
        kitdir = "%s-%s" % (kitname, self.this_docformat)

        mkdir (kitdir)

        [self.__relocate_into (dir=kitdir, part=part) for part in self.o.parts]

        run ("zip -q -r %(kitdir)s.zip %(kitdir)s" % {"kitdir": kitdir})

    # ---------------------
    # -- build_as_needed --
    # ---------------------

    def do_str (self):
        return 'str' in self.o.parts

    def do_tor (self):
        return 'tor' in self.o.parts

    def do_plans (self):
        return 'plans' in self.o.parts

    def do_kit (self):
        return self.o.kitp

    def build_as_needed (self, docformat):

        self.passno += 1
        self.this_docformat = docformat

        mkdir (self.itemsdir())

        if self.do_str() and self.passno == 1:
            self.__prepare_str_dir()
        
        if self.o.testsuite_dir and not self.local_testsuite_dir:
            self.__localize_testsuite_dir ()

        # Build the STR as needed, using the REST generated
        # by prepare_str_dir above:

        if self.do_str():
            self.build_str()

        # Build the TOR as needed, which might look into testsuite results to
        # match TC artifacts against presence of test data dumps:

        if self.do_tor():
            self.build_tor()

        # Build the PLANS as needed:

        if self.do_plans():
            self.build_plans()

        # Build a kit package as queried:

        if self.do_kit():
            qmat.build_kit()

# =======================================================================
# ==                          MAIN SCRIPT BODY                         ==
# =======================================================================

valid_docformats = ('html', 'pdf')
valid_parts      = ('tor', 'plans', 'str')
valid_dolevels   = ('doA', 'doB', 'doC')
valid_xada       = ('95', '2005', '2012')

def commandline():
    """Build and return an OptionParser instance for this script."""

    op = optparse.OptionParser(usage="%prog <options>")

    op.add_option (
        "--root-dir", dest="rootdir",
        help=(
            "Name of a directory where a from-scratch kit construction "
            "will take place. Must not exist already.")
        )
    op.add_option (
        "--work-dir", dest="workdir",
        help=(
            "Name of a directory from where a previous kit construction "
            "will resume. Must exist already.")
        )

    op.add_option (
        "--git-source", dest="gitsource", default=None,
        help=(
            "Git repo we should be cloning to get our source artifacts. "
            "!! This overrides whatever is in a --work-dir already !!"
            )
        )
    op.add_option (
        "--git-pull", dest="gitpull", action="store_true", default=False,
        help=(
            "Pull commits from current origin in the git clone setup "
            "in work-dir."
            )
        )
    op.add_option (
        "--git-reuse", dest="gitreuse", action="store_true", default=False,
        help=(
            "Reuse current git clone setup in work-dir, as-is. "
            )
        )
    op.add_option (
        "--branch", dest="branchname", default="opendo",
        help = (
            "The git branch we shall produce the material from.")
        )

    op.add_option (
        "--docformat", dest="docformat",
        type='string', # choices=valid_docformats,
        help = (
            "The format we need to produce for each document. "
            "At least one of %s. Multiple values separated by ','." \
                % valid_docformats.__str__())
        )
    op.add_option (
        "--parts", dest="parts", default=None,
        help = (
            "A comma separated list of the parts of the qualkit that "
            "are to be generated, subset of %s." % valid_parts.__str__())
        )

    op.add_option (
        "--dolevel", dest="dolevel", default=None,
        type='choice', choices=valid_dolevels,
        help = (
            "Target DO178 qualification level. One of %s." \
                % valid_dolevels.__str__())
        )

    op.add_option (
        "--testsuite-dir", dest="testsuite_dir", default=None,
        help = (
            "Name of a directory where the testsuite was run")
        )

    op.add_option (
        "--rekit", dest="rekit", default=None,
        help = (
            "rebuild the specified --parts and re-package them in the "
            "kit stamped according to this option's value (YYYYMMDD)")
        )

    op.add_option (
        "--xgnatpro", dest="xgnatpro", default=None,
        help = (
            "Version we expect <target>-gcc -v to match. "
            "Mandatory when producing a kit")
        )
    op.add_option (
        "--xgnatcov", dest="xgnatcov", default=None,
        help = (
            "Version we expect gnatcov --version to match. "
            "Mandatory when producing a kit")
        )
    op.add_option (
        "--xgnatemu", dest="xgnatemu", default=None,
        help = (
            "Version we expect <target>-gnatcov --version to match.")
        )

    op.add_option (
        "--xada", dest="xada", default=None, choices=valid_xada,
        help = (
            "Version of the Ada language we are qualifying for (%s)" % \
                '|'.join(valid_xada)
            )
        )

    return op

def check_valid(options, args):

    # We are producing qualification material. Better know what we're
    # aiming at, always:

    exit_if (
        not options.dolevel,
        "Please specify an explicit dolevel (--dolevel)."
        )

    # Generating docs can be pretty long. Better make sure the output format
    # was intentionally stated:

    exit_if (
        not options.docformat,
        ("Please specify the desired output format (--docformat).")
        )

    # work dir vs root dir.

    exit_if (
        not options.workdir and not options.rootdir,
        "A root work dir must be specified (--root-dir or --work-dir)"
        )

    exit_if (
        options.workdir and options.rootdir,
        "--root-dir and --work-dir may not be combined together."
        )

    exit_if (
        options.rootdir and os.path.exists (options.rootdir),
        "The --root-dir location (%s) must not exist already" \
            % options.rootdir
        )

    # Convey whether we are requested to produce a kit:

    options.kitp = options.rekit or not options.parts

    # Settle on the set of documents we are to produce:

    options.parts = (
        valid_parts if not options.parts
        else options.parts.split(',')
        )

    [exit_if (
            part not in valid_parts,
            "Requested part '%s' is invalid, none of %s" \
                % (part, valid_parts.__str__())
            )
     for part in options.parts]

    # Producing a STR requires a testsuite dir

    exit_if (
        'str' in options.parts and not options.testsuite_dir,
        "--testsuite-dir required when producing a STR"
        )

    # GIT aspects:

    exit_if (
        options.gitpull and options.gitsource,
        "Specifying git source is incompatible with "
        "request to pull from current origin"
        )

    # -xada, -xgnatcov etc

    # ??? We need access to the suite context data for these checks.
    # We don't always have it handy and fixing this requires moving to
    # a dump format amenable to data exchange across hosts and python
    # versions. We use pickle today, which doesn't fit the bill.


if __name__ == "__main__":

    (options, args) = commandline().parse_args()

    check_valid (options, args)

    # Make sure that directory options are absolute dirs, to
    # facilitate switching back and forth from one to the other:

    if options.testsuite_dir and not raccess_in (options.testsuite_dir):
        options.testsuite_dir = os.path.abspath (options.testsuite_dir)

    # Instanciate our helper and proceed with the base directory setup:

    qmat = QMAT (options=options)

    qmat.setup_workdir()
    qmat.git_update()
    qmat.switch_to_branch()

    # Produce each part we are requested to produce, with a tailored
    # QM model:

    qmat.gen_qm_model()

    # Build the various parts and maybe the kit for each requested format:

    [qmat.build_as_needed (docformat=f) for f in options.docformat.split(',')]

    if options.kitp:
        qmat.dump_kit_consistency_log()
