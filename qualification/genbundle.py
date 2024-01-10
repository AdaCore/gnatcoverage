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
#          by the QM from artifacts stored in a dedicated subdirectory
#
# * TOR  : the GNATcoverage "Tool Operational Requirements" document, tree
#          of requirements and accompanying testcase descriptions produced
#          by the QM from artifacts stored within the testsuite tree
#
# * STR  : the GNATcoverage "Software Test Results" report, summary of
#          a qualification testsuite execution for a specific configuration
#          (this target, these tool versions with such options, producing
#          such and such testsuite results)
#
# All the artifacts required to produce these documents are hosted in a GIT
# repository. This ought to be the AdaCore main "gnatcoverage-qualification"
# repository for material to be delivered.
#
# The three documents may be produced in either html or pdf format. Most of
# the time, this is achieved by using Sphinx on generated REST.
#
# Packaging essentially consists in setting up a subdir where each document
# has a dedicated place and creating a zip archive.
#
# ---
#
# An execution of this script typically proceeds with the following steps:
#
# 1) Set up a git clone of the repo where the TOR and PLANS artifacts
#    are located (helps making sure we don't accidentally incorporate local
#    changes not checked-in)
#
# 2) Build whatever is requested, using PLANS and TOR artifact from the
#    locally setup git repo and STR artifacts from a directory where the
#    testsuite execution took place, designated with --testsuite-dir.
#
#    This produces the html or pdf (according to --docformat) documents of
#    interest + stuff we don't care about, e.g. intermediate latex sources for
#    rest->pdf,
#
# 3) Move or copy the final documents in a subdir named after their format
#    (PDF|HTML subdir), then maybe build an archive of the set of available
#    items for each format.
#
# Everything takes place in a "root" or "work" directory, specified with
# --work-dir. The designated dir is created if it doesn't exist, and is reused
# as-is otherwise. Care must be taken when reusing a work-dir and producing
# kits for delivery purposes, as this script is not designed to guarantee 100%
# accurate results in all situations involving restarts after a number of
# stops at arbitrary points.
#
# The target dir might already have a clone setup. By default, a selected git
# source is re-cloned there. --git-reuse and --git-pull provide alternate
# options, sometimes handy during adjustment phases.
#
# When cloning in a work dir that doesn't have a clone already, or when
# re-cloning when neither --git-reuse nor --git-pull is requested,
# --git-source lets you state which repo is to be cloned. In absence of an
# explicit source, the main AdaCore git repo for GNATcoverage is selected.
#
# During pure development phases, --git-rsync /your/local/gnatcoverage
# will let you try out the production of documents without even committing
# them to the local repo.
#
# Example development sequence:
# =============================
#
# Setting up a root dir from a clone of a local repo, switching to
# the "dev-str" working branch, building plans in html format for starters:
#
#   python genbundle.py --docformat=html
#     --work-dir=$HOME/my-qmat
#     --git-source=$HOME/gnatcoverage --branch=dev-str
#     --parts=plans
#
# Testing plans regeneration after local commit, pulled in the cloned repo:
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
# During pure development phases, testing STR production using an
# rsynced copy of your local checkout, possibly including changes not
# yet committed:
#
#  python genbundle.py --docformat=html,pdf --parts=str --dolevel doA
#  --testsuite-dir=$HOME/gnatcoverage/testsuite
#  --work-dir=$HOME/my-qmat
#  --git-rsync=$HOME/gnatcoverage --branch=qual-quantified-expr

# Testsuite directories
# =====================
#
# When producing STR, a few steps take place:
#
# * The designated testsuite-dir first is populated with REST sources
#   describing the run results, collecting test results from the testuite
#   tree at the location where the run took place.
#
# * A local copy of the full testsuite tree (subdir where the testsuite
#   execution took place) is fetched within the work-dir.
#
# * A pdf/html STR report is produced from there with sphinx.
#
# Note that:
#
# * --testsuite-dir supports "remote access" prefixes like "[login@]hostname:"
#
# * All the steps described above take place for both local and remote
#   designated directories.
#
# * The directory name for the local copy of the testsuite-dir is computed as
#   the sha1 hashed value of the designated directory name.
#
# Repositories involved
# =====================
#
# The host where the tests have to run sometimes differs from the host where
# the kit gets produced, e.g. when the target operationsal environment is an
# OS where the QM isn't available.
#
# There are then multiple repositories involved in a typical kit production:
#
# On the host where the qualification testsuite run takes place:
# --------------------------------------------------------------
#
# [source-repo-for-testsuite-run]
#     |
#     | (clone by user to run testsuite)
#     v
#     gnatcoverage/testsuite/testsuite.py
#                           /Qualif/        <- testbase & STR artifacts
#
# On the host where the kit production is launched:
# -------------------------------------------------
#
# [source-repo-for-document-artifacts]
#             |
#             | (clone by genbundle.py)
#             v
#  $work-dir/ gnatcoverage-git-clone/
#                   qualification/qm/plans/ <- PLANS artifacts
#                   testsuite/              <- TOR artifacts
#
# [source-repo-for-kit-production-setup]
#     |
#     | (clone by user to launch kit production)
#     v
#     gnatcoverage/qualification/genbundle.py
#
# For kits to be delivered, the three source-repos typically are the same and
# operations are performed from the same branch therein, ideally from
# identical commits. In practice minor corrections are often included in the
# doc artifacts after the testsuite run and we don't re-execute everything
# just for this. This script produces a consistency log about the relative
# positions of the tree from which the tests were run compared to the tree
# from which the PLANS and TOR documents are produced.
#
# Example kit production commands:
# ================================
#
# Producing a packaged kit, with a toplevel index and a final .zip archive is
# achieved by not restricting to specific parts, hence by not passing --parts
# at all. A DO level has to be provided in this case:
#
#   python genbundle.py --docformat=html
#     --workdir=$HOME/my-qmat
#     --branch=<project-branch>
#     --dolevel=doA
#     --testsuite-dir=<...>
#
# *****************************************************************************

from e3.os.process import Run
from e3.fs import cp, mv, rm, mkdir, ls, find

from datetime import date

import optparse, sys, os.path, shutil, re, hashlib

# This lets us access modules that the testuite
# code features:
MY_TESTSUITE_DIR=os.path.abspath("../testsuite")
sys.path.append(MY_TESTSUITE_DIR)

from SUITE.qdata import CTXDATA_FILE, treeref_at
from SUITE.cutils import contents_of, output_of
from SUITE.dutils import jload_from

# =======================================================================
# ==                         MISC UTILITY FUNCTIONS                    ==
# =======================================================================

class Error (Exception):
    def __init__(self):
        pass

def fail_if (p, msg):
    if p:
        print(msg)
        raise Error

def exit_if (p, msg):
    if p:
        print(msg)
        sys.exit(1)

def warn_if (p, msg):
    if p:
        print("\n!!! WARNING: %s !!!\n" % msg)

def run_list (cmd, dir=None, env=None):
    """Execute the provided CMD command-list (command name + arguments),
    temporarily switching to the DIR directory unless None, dumping a .log
    file tracking the command output in the directory where the command
    executes. ENV, if not none, is expected to hold a dictionary of
    environment variable values to be made visible to the executed command, on
    top of os.environ."""

    oriwd = os.getcwd()
    print("from : %s" % oriwd)

    if dir:
        print("hopat : %s" % dir)
        os.chdir (dir)

    print("run  : %s" % ' '.join(cmd))

    out = os.path.basename(cmd[0])+".log"
    p = Run (cmd, output=out, env=env, ignore_environ=False)

    fail_if (
        p.status != 0, "execution failed\n"
        + "log was:\n" + contents_of(out))

    os.chdir (oriwd)

def run (s, dir=None, env=None):
    run_list (s.split(), dir, env=env)

def announce (s):
    print("=========== " + s)

def remove (path):
    """Delete the file or directory subtree designated by PATH"""

    print("from : %s" % os.getcwd())
    print("remove : %s" % path)

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

    def isdir(path):
        return os.path.isdir(path) and not os.path.islink(path)

    if os.path.exists (local_name):
        rm (local_name, recursive=isdir(local_name))

    if os.path.exists (path):
        mv (path, local_name)
        rm (local_name, recursive=isdir(local_name))

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
GIT_MASTER = "git@ssh.gitlab.adacore-it.com:eng/das/cov/gnatcoverage-qualification.git"

# The subdir name for this clone, relative to --root
GIT_CLONE_SUBDIR = "gnatcoverage-git-clone"

class QMAT:

    def itemsdir (self):
        return os.path.join (self.workdir, "%s" % self.this_docformat.upper())

    def __init__(self, options):

        self.o = options

        # Make sure we can designate the work dir easily from anywhere.
        # Latch repo dir location while we're at it:

        self.workdir = os.path.abspath (options.workdir)
        self.repodir = os.path.join (self.workdir, GIT_CLONE_SUBDIR)

        # A local place where the testsuite tree may be found,
        # possibly after remote syncing from testsuite_dir if that
        # is non-local:

        self.local_testsuite_dir = None

        # To be set prior to build_as_needed():

        self.this_docformat = None

        # Sequence number of doc build pass we are processing:

        self.passno = 0


    def process_imports(self, dir):
        """
        Process the template file in dir, replacing the occurrences of
        <%name%> in the text by the contents of name_<trace_mode>.rst,
        according to the qualification parameters
        """
        def replace_one(p):
            return contents_of(
                os.path.join(dir,p.group(1) + f"_{self.o.trace_mode}.rst")
        )


        template_content = contents_of(os.path.join (dir,"content.tmpl"))
        with open(os.path.join(dir, "content.rst"), "w") as f:
            f.write(re.sub(
                pattern=r"<%([^%]+)%>",
                repl=replace_one,
                string=template_content
            ))

    # ---------------------
    # -- prepare_content --
    # ---------------------

    def prepare_content(self, dirs):
        """
        Prepare the content files by choosing the correct content_*.rst file
        according to the qualification parameters.
        """
        for dir in dirs:
            cp(
                os.path.join(dir, f"content_{self.o.trace_mode}.rst"),
                os.path.join(dir, f"content.rst"),
            )

    # --------------------
    # -- setup_workdir --
    # --------------------

    def setup_workdir (self):

        announce ("setting up working dir at %s" % self.workdir)

        mkdir (self.workdir)


    # ---------
    # -- log --
    # ---------

    def log (self, line):
        with open (os.path.join(self.workdir, "genbundle.log"), 'a') as f:
            f.write (line + '\n')

    # ----------------
    # -- git_update --
    # ----------------

    def git_update (self):

        # If we're requested to rsync from an existing repo dir, do so

        if self.o.gitrsync:
            run_list (
                ["rsync", "-ar", self.o.gitrsync + '/', self.repodir + '/',
                 '--delete', '--delete-excluded',
                 '--filter', '. dev.rsync',
                ]
            )
            return

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

        os.chdir(self.workdir)

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

        print("%s %s available in %s %s" % (
            self.this_docformat, part.upper(),
            this_target, "(toplevel)" if toplevel else ""
            ))

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
                 " genmodel.py --dolevel=%s --languages=%s" %
             (self.o.dolevel, self.o.languages))

    # ----------------
    # -- __qm_build --
    # ----------------

    def __qm_build (self, part):
        """Build one PART of using the Qualifying Machine."""

        announce ("building %s %s" % (self.this_docformat, part.upper()))

        os.chdir (
            os.path.join (self.repodir, "qualification", "qm")
            )

        # The qmachine model might use the "build" directory as
        # a repository, and it has to preexist:

        mkdir ("build")
        run ("qmachine model.xml -l scripts/generate_%s_%s.py" \
                 % (part, self.this_docformat),
             env={'GENBUNDLE_DOLEVEL': self.o.dolevel})

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

            def sync(tr):
                target_dir = os.path.join (
                    self.repodir, "testsuite", os.path.dirname (tr)
                    )
                if os.path.exists(target_dir):
                    cp (tr, target_dir)
                else:
                    print ("ERRRR !! inexistant target dir for %s" % tr)

            [sync(tr) for tr in find (root=".", pattern="tc.dump")]
        env_chapter_dir = os.path.join(self.repodir, "testsuite", "Qualif", "Environment")
        self.process_imports(env_chapter_dir)

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
        suite_treeref = suite_ctxdata['treeref']

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
            actual = suite_ctxdata['gnatpro']['version'],
            expected = self.o.xgnatpro)

        check_one (
            tool = "gnatcov",
            actual = suite_ctxdata['gnatcov']['version'],
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

        logfile = os.path.join(self.workdir, "consistency.log")
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

        suite_ctxdata = jload_from (
            os.path.join (self.local_testsuite_dir, CTXDATA_FILE))

        self.__dump_tree_consistency_info (log, suite_ctxdata)
        self.__dump_version_consistency_info (log, suite_ctxdata)
        self.__dump_tr_consistency_info (log)

        log.close()

        print("consistency log available at %s" % logfile)

    # ----------------------------
    # -- localize_testsuite_dir --
    # ----------------------------

    def __localize_testsuite_dir (self):
        """If testsuite_dir is remote, fetch a local copy.  Memorize the local
        location always."""

        os.chdir (self.workdir)

        self.local_testsuite_dir = \
            hashlib.sha1(self.o.testsuite_dir.encode()).hexdigest()

        # Exclude non qualification tests and internal reports aimed at our
        # intranet from the transfer. All useless and potentially confusing.
        # Also get rid of binaries, including executables without extensions.

        # First rsync rule to hit wins, so the idea is to exclude patterns we
        # know we want out, then include every file with a '.' (.out, .adb,
        # ...), then every directory, then exclude everything. Preserve the
        # STR subdir contents entirely. There's no binary there, and artifacts
        # we all need to produce the STR after the copy (e.g. Makefile).

        run ("rsync -arz --delete --delete-excluded %s/ %s %s" % (
                self.o.testsuite_dir, self.local_testsuite_dir,
                ' '.join (("--exclude=/tests",
                           "--exclude=/output",
                           "--exclude=/rep_gnatcov*",
                           "--exclude=*.o",
                           "--exclude=*.obj",
                           "--exclude=*.exe",
                           "--exclude=*.trace",
                           "--exclude=*.dmap",
                           "--include=*.*",
                           "--include=*/",
                           "--include=/STR/**",
                           "--exclude=*"))))

        self.log (
            "testsuite-dir %s fetched as %s" % (
                self.o.testsuite_dir, self.local_testsuite_dir)
            )

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
        plans_root = os.path.join(self.repodir, "qualification", "qm", "plans")
        trace_specific_content = [
            os.path.join(
                plans_root,
                "Tool_Qualification_Plan",
                "Qualified_Interface"
            )
        ]
        self.prepare_content(trace_specific_content)
        self.__qm_build (part="plans")

    # ---------------
    # -- build_kit --
    # ---------------

    def __relocate_into(self, dir, part):

        the_item = self.kititem_for(part=part)

        item_source_path = os.path.join (self.itemsdir(), the_item)
        item_target_path = os.path.join (dir, the_item)

        remove (item_target_path)

        print("move : %s" % item_source_path)
        print("into : %s" % dir)

        mv (item_source_path, dir)

    def build_kit (self):
        announce ("building %s kit" % self.this_docformat)

        os.chdir (self.workdir)

        # The kit name is computed as:
        #
        #    gnatcov-qualkit-<kitid>-<YYYYMMDD>
        #
        # where <YYYYMMDD> is the kit production stamp (now), and <kitid> is
        # computed from the git branch off which the artifacts are taken. The
        # git branch name might contain the "qualkit" indication already.

        today = date.today()
        gitbranch = current_gitbranch_at(self.repodir)

        kitprefix = (
            "gnatcov-qualkit" if "qualkit" not in gitbranch
            else "gnatcov"
            )

        kitid = gitbranch
        kitid = kitid.replace('/', '-')
        kitid = kitid.replace('.', '_')

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

    # -----------------------
    # -- zip_testsuite_dir --
    # -----------------------

    def zip_testsuite_dir(self):

        os.chdir (self.workdir)

        relative_testsuite_dir = os.path.basename (self.local_testsuite_dir)

        zipname = "%s.zip" % relative_testsuite_dir

        remove (zipname)
        run ("zip -q -r %s %s" % (zipname, relative_testsuite_dir))

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

valid_docformats  = ('html', 'pdf')
valid_parts       = ('tor', 'plans', 'str')
valid_dolevels    = ('doA', 'doB', 'doC')
valid_xada        = ('95', '2005', '2012')
valid_languages   = ["Ada%s" % version for version in valid_xada]
valid_trace_modes = ("src", "bin")

def commandline():
    """Build and return an OptionParser instance for this script."""

    op = optparse.OptionParser(usage="%prog <options>")

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
        "--git-rsync", dest="gitrsync", default=False,
        help=(
            "Rsync an existing git repo into our git clone dir."
            )
        )
    op.add_option (
        "--branch", dest="branchname", default=None,
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
        "--languages", dest="languages", default=None,
        help = (
            "Comma separated list of languages, for traceability matrix "
            "purposes. Amongst %s." % valid_languages.__str__())
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

    op.add_option (
        "--trace-mode",
        dest="trace_mode",
        default=None,
        choices=valid_trace_modes,
        help="Trace kind we are qualifying gnatcov for. One of 'src' or 'bin'"
    )

    return op

def check_valid(options, args):

    # We are producing qualification material. Better know what we're
    # aiming at, always:

    exit_if (
        not options.dolevel,
        "Please specify an explicit dolevel (--dolevel)."
        )

    exit_if (
        not options.trace_mode,
        "Please specify an explicit trace-mode (--trace-mode)."
        )

    # Generating docs can be pretty long. Better make sure the output format
    # was intentionally stated:

    exit_if (
        not options.docformat,
        ("Please specify the desired output format (--docformat).")
        )

    # Likewise for the git branch name:

    exit_if (
        not options.branchname,
        ("Please specify the git branch name (--branch).")
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

    # work dir

    exit_if (
        not options.workdir,
        "A work dir must be specified (--work-dir)"
        )

    warn_if (
        options.kitp and os.path.exists (options.workdir)
        and ls("%s/*" % options.workdir),
        "producing kit within non empty workdir")

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

    # In case we produce TOR/LRM traceability matrixes ...

    exit_if (
        not options.languages,
        "Please specify the qualified languages (--languages)"
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
        qmat.zip_testsuite_dir()
        qmat.dump_kit_consistency_log()
