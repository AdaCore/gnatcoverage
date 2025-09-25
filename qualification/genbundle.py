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
#  $work-dir/ <REPO_IMAGE_SUBDIR>
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
from e3.fs import cp, mv, rm, mkdir, ls, find, sync_tree

from datetime import date

import hashlib
import optparse
import os.path
import re
import sys
import yaml

# This lets us access modules that the testsuite
# code features:
MY_TESTSUITE_DIR = os.path.abspath("../testsuite")
sys.path.append(MY_TESTSUITE_DIR)

from SUITE.qdata import CTXDATA_FILE, treeref_at  # noqa: E402
from SUITE.cutils import contents_of, output_of  # noqa: E402
from SUITE.dutils import jload_from  # noqa: E402

# =======================================================================
# ==                         MISC UTILITY FUNCTIONS                    ==
# =======================================================================


class Error(Exception):
    def __init__(self):
        pass


def fail_if(p, msg):
    if p:
        print(msg)
        raise Error


def exit_if(p, msg):
    if p:
        print(msg)
        sys.exit(1)


def warn_if(p, msg):
    if p:
        print("\n!!! WARNING: %s !!!\n" % msg)


def run_list(cmd, dirname=None, env=None):
    """Execute the provided CMD command-list (command name + arguments),
    temporarily switching to the DIR directory unless None, dumping a .log
    file tracking the command output in the directory where the command
    executes. ENV, if not none, is expected to hold a dictionary of
    environment variable values to be made visible to the executed command, on
    top of os.environ."""

    oriwd = os.getcwd()
    print("from : %s" % oriwd)

    if dirname:
        print("hopat : %s" % dirname)
        os.chdir(dirname)

    print("run  : %s" % " ".join(cmd))

    out = os.path.basename(cmd[0]) + ".log"
    p = Run(cmd, output=out, env=env, ignore_environ=False)

    fail_if(
        p.status != 0, "execution failed\n" + "log was:\n" + contents_of(out)
    )

    os.chdir(oriwd)


def run(s, dirname=None, env=None):
    run_list(s.split(), dirname=dirname, env=env)


def announce(s):
    print("=========== " + s)


def remove(path):
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

    if os.path.exists(local_name):
        rm(local_name, recursive=isdir(local_name))

    if os.path.exists(path):
        mv(path, local_name)
        rm(local_name, recursive=isdir(local_name))


# testsuite dirs may include a [user@]remote-host: prefix


def raccess_in(path):
    components = path.split(":")
    return components[0] if len(components) > 1 else None


def rdir_in(path):
    components = path.split(":")
    return components[1] if len(components) > 1 else None


# current git branch name, used as kit identifier:


def current_gitbranch_at(dirname):
    cwd = os.getcwd()
    os.chdir(dirname)

    this_branch = re.search(
        pattern=r"\* (?P<branchname>\S*)", string=output_of("git branch")
    ).group("branchname")

    os.chdir(cwd)

    return this_branch


# =======================================================================
# ==              QUALIF MATERIAL GENERATION HELPER CLASS              ==
# =======================================================================

sphinx_target_for = {"html": "html", "pdf": "latexpdf"}

# The master GIT repo where our source artifacts reside
GIT_MASTER = os.environ.get("GIT_MASTER")

# The name of the subdir in the working directory where we'll fetch TOR
# artifacts from. This would be an image of a "gnatcoverage" repository, from
# either
#
# - the immediate result of a git clone command, or
# - an rsync from a cloned repo somewhere (--rsync <path/to/git-clone>),
# - an rsync from a source package, typically prepared from a branch
#   for a qualification project (--rsync <path/to/src_pkg>).

REPO_IMAGE_SUBDIR = "gnatcoverage-git-clone"


class QMAT:
    def itemsdir(self):
        return os.path.join(self.workdir, "%s" % self.this_docformat.upper())

    def __init__(self, options):
        self.o = options

        # Make sure we can designate the work dir easily from anywhere.
        # Latch repo dir location while we're at it:

        self.workdir = os.path.abspath(options.workdir)
        self.repodir = os.path.join(self.workdir, REPO_IMAGE_SUBDIR)
        self.qualdir = os.path.join(self.repodir, "qualification", "qualkit")

        self.settings = {}
        self.doc_versions = {}

        # A local place where the testsuite tree may be found,
        # possibly after remote syncing from testsuite_dir if that
        # is non-local:

        self.local_testsuite_dir = ""

        # To be set prior to build_as_needed():

        self.this_docformat: str = ""

        # Sequence number of doc build pass we are processing:

        self.passno = 0

    def get_doc_version(self, part):
        """
        Retrieve the document version string for the part document defined
        in the document_settings.yaml file.
        """
        doc_settings_filename = os.path.join(
            self.qualdir,
            part,
            "document_settings.yaml",
        )
        with open(doc_settings_filename) as fd:
            for line in fd.readlines():
                match_res = re.search(r"doc_version: +'(\d+)'", line)
                if match_res:
                    return match_res.group(1)

    def load_settings(self):
        """
        Load project settings from the yaml common file
        """
        settings_file = os.path.join(
            self.qualdir,
            "_common",
            "project_settings.yaml",
        )
        with open(settings_file) as fd:
            self.settings = yaml.safe_load(fd)
        for part in ["str", "plans", "tor"]:
            self.doc_versions[part] = self.get_doc_version(part)

    # ---------------------
    # -- prepare_content --
    # ---------------------

    def prepare_content(self, dirs, root):
        """
        Prepare the content files by choosing the correct *_content.rst file
        according to the qualification parameters.
        """
        for dirname in dirs:
            cp(
                os.path.join(
                    root, dirname, f"{self.o.trace_mode}_content.rst"
                ),
                os.path.join(root, dirname, "content.rst"),
            )

    # --------------------
    # -- setup_workdir --
    # --------------------

    def setup_workdir(self):
        announce("setting up working dir at %s" % self.workdir)

        mkdir(self.workdir)

    # ---------
    # -- log --
    # ---------

    def log(self, line):
        with open(os.path.join(self.workdir, "genbundle.log"), "a") as f:
            f.write(line + "\n")

    # ----------------
    # -- rsync_from --
    # ----------------

    def rsync_from(self, rsync_source):
        run_list(
            [
                "rsync",
                "-ar",
                rsync_source + "/",
                self.repodir + "/",
                "--delete",
                "--delete-excluded",
                "--filter",
                ". dev.rsync",
            ]
        )

    # ----------------
    # -- git_update --
    # ----------------

    def git_update(self):
        # If we're requested to pull/update only, do so

        if self.o.gitpull:
            announce("updating git clone from origin")

            os.chdir(self.repodir)
            run("git pull --rebase origin")
            return

        # If we're requested to reuse an existing clone, do so

        if self.o.gitreuse:
            announce("reusing existing git clone")
            return

        # Otherwise, get a fresh clone.

        os.chdir(self.workdir)

        gitref = self.o.gitsource if self.o.gitsource else GIT_MASTER
        exit_if(
            not gitref,
            "set the repository source either through the --git-source option"
            " or through the GIT_MASTER environment variable",
        )

        announce("cloning git repository from %s" % gitref)

        remove(REPO_IMAGE_SUBDIR)
        run("git clone %s %s" % (gitref, REPO_IMAGE_SUBDIR))

    # ----------------------
    # -- switch_to_branch --
    # ----------------------

    def switch_to_branch(self):
        announce("switching to branch '%s'" % self.o.branchname)

        os.chdir(self.repodir)
        run("git checkout %s" % self.o.branchname)

    # -----------------
    # -- kititem_for --
    # -----------------

    def kititem_for(self, part):
        """The name of the filesystem entity which materializes PART in a kit
        distribution.  This will be a subdirectory name for treeish formats
        a-la html (e.g. TOR, which will contain content.html etc), or a
        specific filename (e.g. PLANS-<doc_id>.pdf) for other formats. This is
        what latch_into sets up eventually."""

        this_item_is_tree = self.this_docformat == "html"

        this_item_name = (
            self.settings["project"]["subst"][f"{part.lower()}_abb"].strip("*")
        )

        this_item_id = self.settings["project"]["subst"][f"{part.lower()}_id"]

        this_item_suffix = (
            "" if this_item_is_tree else ".%s" % self.this_docformat
        )

        return (
            f"{this_item_name}_{this_item_id}_v{self.doc_versions[part]}"
            .upper().replace(".", "_")
            + this_item_suffix
        )

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

    def __latch_into(self, dirname, part, toplevel, copy_from=None):
        this_target_is_tree = self.this_docformat == "html"

        # Compute the target dir or file name for our copy:
        this_target = (
            dirname
            if toplevel and this_target_is_tree
            else os.path.join(dirname, self.kititem_for(part=part))
        )

        # Compute the source dir or file name for our copy:

        # The local or provided source build subdir name, assuming a
        # sphinx setup, with an html or pdf sub-subdir depending on the
        # doc format. For file outputs, assume the builders are setup to
        # produce PART.<docformat>, e.g. TOR.pdf:

        this_build_subdir = os.path.join(
            copy_from if copy_from is not None else "build",
            self.this_docformat,
        )

        this_source = (
            this_build_subdir
            if this_target_is_tree
            else os.path.join(
                this_build_subdir, part.lower() + ".%s" % self.this_docformat
            )
        )

        # Delete an old version of latched results that might
        # already be there if we're running with --work-dir.

        remove(this_target)

        # Now proceed with the latch operation per se:

        if not this_target_is_tree:
            cp(this_source, this_target, recursive=False)

        elif copy_from:
            cp(this_source, this_target, recursive=True)

        else:
            mv(this_build_subdir, this_target)

        print(
            "%s %s available in %s %s"
            % (
                self.this_docformat,
                part.upper(),
                this_target,
                "(toplevel)" if toplevel else "",
            )
        )

    # ---------------
    # -- build_tor --
    # ---------------

    def build_tor(self):
        # If we have a local testsuite dir at hand and we haven't done so
        # already, fetch the tests results that the QM needs to check TOR/TC
        # consistency:

        if self.local_testsuite_dir and self.passno == 1:
            os.chdir(self.local_testsuite_dir)

            def sync(tr):
                target_dir = os.path.join(
                    self.repodir, "testsuite", os.path.dirname(tr)
                )
                if os.path.exists(target_dir):
                    cp(tr, target_dir)
                else:
                    print("ERROR !! inexistent target dir for %s" % tr)

            [sync(tr) for tr in find(root=".", pattern="tcs.dump")]

        announce("building %s %s" % (self.this_docformat, "TOR"))

        tor_dir = os.path.join(self.repodir, "qualification", "qualkit", "tor")
        run(
            f"python genrest.py --dolevel {self.o.dolevel} --force"
            f" --testsuite-dir {self.o.testsuite_dir}",
            env={"PYTHONPATH": self.qualdir},
            dirname=tor_dir,
        )
        run("make %s" % (self.this_docformat), dirname=tor_dir)

        self.__latch_into(
            dirname=self.itemsdir(),
            part="tor",
            toplevel=False,
            copy_from=os.path.join(tor_dir, "build"),
        )

    # ------------------------------
    # -- dump_kit_consistency_log --
    # ------------------------------

    def __dump_tree_consistency_info(self, log, suite_ctxdata):
        log.write("\n-- TREE CONSISTENCY LOG:\n")

        if not suite_ctxdata:
            log.write(
                "suite context info unavailable, "
                "tree consistency unchecked\n"
            )
            return

        os.chdir(self.repodir)

        local_treeref = treeref_at(self.repodir)
        suite_treeref = suite_ctxdata["treeref"]

        if local_treeref == suite_treeref:
            log.write(
                "artifact tree ref matches testsuite (%s)\n" % local_treeref
            )
            return

        log.write(
            "local_treeref = %s , suite_treeref = %s\n"
            % (local_treeref, suite_treeref)
        )

        merge_base = output_of(
            "git merge-base %s %s" % (local_treeref, suite_treeref)
        ).strip(" \n")

        (first, next_item) = (
            (suite_treeref, local_treeref)
            if merge_base == suite_treeref
            else (
                (local_treeref, suite_treeref)
                if merge_base == local_treeref
                else (None, None)
            )
        )

        if first is None:
            log.write("!!! local and suite tree refs aren't sequential !!!\n")
            return

        log.write(
            "%s tree is ahead\n"
            % ("suite" if first == local_treeref else "local")
        )

        gitlog_cmd = "git rev-list --oneline %s ^%s" % (next_item, first)
        log.write("\n".join(["", gitlog_cmd, "--", output_of(gitlog_cmd), ""]))

    def __dump_version_consistency_info(self, log, suite_ctxdata):
        log.write("\n-- VERSION CONSISTENCY LOG:\n")

        if not suite_ctxdata:
            log.write(
                "suite context info unavailable, "
                "versions consistency unchecked\n"
            )
            return

        def check_one(tool, actual, expected):
            if not expected:
                log.write("expected %s version NOT specified\n" % tool)
                return

            note = (
                "matches"
                if re.search(pattern=expected, string=actual)
                else "doesn't match"
            )

            log.write(
                '%(tool)s version "%(actual)s" %(note)s '
                'expectation "%(expected)s"\n'
                % {
                    "tool": tool,
                    "note": note,
                    "expected": expected,
                    "actual": actual,
                }
            )

        #

        check_one(
            tool="gnatpro",
            actual=suite_ctxdata["gnatpro"]["version"],
            expected=self.o.xgnatpro,
        )

        check_one(
            tool="gnatcov",
            actual=suite_ctxdata["gnatcov"]["version"],
            expected=self.o.xgnatcov,
        )

    def __dump_tr_consistency_info(self, log):
        log.write("\n-- TOR/TR CONSISTENCY LOG:\n")

        tor_tr_logfile = os.path.join(
            self.repodir,
            "qualification",
            "qualkit",
            "tor",
            "missing_tr_log.txt",
        )

        if not os.path.exists(tor_tr_logfile):
            log.write("QM log NOT available\n")
            return

        log.write(
            "%s TOR/TR consistency log from QM @ %s :\n"
            % ("FRESH" if self.do_tor() else "OLD", tor_tr_logfile)
        )
        log.write(contents_of(tor_tr_logfile))
        log.write("--\n")

    def dump_kit_consistency_log(self):
        announce("dumping consistency log - format agnostic")

        logfile = os.path.join(self.workdir, "consistency.log")
        log = open(logfile, "w")

        log.write(
            "artifact tree at %s (%s)\n"
            % (self.repodir, treeref_at(self.repodir))
        )
        log.write(
            "designated testsuite tree at %s (%s)\n"
            % (
                self.o.testsuite_dir,
                "REMOTE" if raccess_in(self.o.testsuite_dir) else "local",
            )
        )
        log.write("local testsuite tree at %s\n" % self.local_testsuite_dir)

        suite_ctxdata = jload_from(
            os.path.join(self.local_testsuite_dir, CTXDATA_FILE)
        )

        self.__dump_tree_consistency_info(log, suite_ctxdata)
        self.__dump_version_consistency_info(log, suite_ctxdata)
        self.__dump_tr_consistency_info(log)

        log.close()

        print("consistency log available at %s" % logfile)

    # ----------------------------
    # -- localize_testsuite_dir --
    # ----------------------------

    def __localize_testsuite_dir(self):
        """If testsuite_dir is remote, fetch a local copy.  Memorize the local
        location always."""

        os.chdir(self.workdir)

        self.local_testsuite_dir = hashlib.sha1(
            self.o.testsuite_dir.encode()
        ).hexdigest()

        # Exclude non qualification tests and internal reports aimed at our
        # intranet from the transfer. All useless and potentially confusing.
        # Also get rid of binaries, including executables without extensions.

        # First rsync rule to hit wins, so the idea is to exclude patterns we
        # know we want out, then include every file with a '.' (.out, .adb,
        # ...), then every directory, then exclude everything. Preserve the
        # STR subdir contents entirely. There's no binary there, and artifacts
        # we all need to produce the STR after the copy (e.g. Makefile).

        run(
            "rsync -arz --delete --delete-excluded %s/ %s %s"
            % (
                self.o.testsuite_dir,
                self.local_testsuite_dir,
                " ".join(
                    (
                        "--exclude=/tests",
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
                        "--exclude=*",
                    )
                ),
            )
        )

        self.log(
            "testsuite-dir %s fetched as %s"
            % (self.o.testsuite_dir, self.local_testsuite_dir)
        )

        self.local_testsuite_dir = os.path.abspath(self.local_testsuite_dir)

    # ---------------------
    # -- prepare_str_dir --
    # ---------------------

    def __prepare_str_dir(self):
        """Helper for build_str. If we haven't done it already, arrange to
        generate the STR REST for the designated testsuite-dir, possibly
        remote.  Fetch this dir back as needed then and remember where the
        corresponding STR subdir (with REST generated) is located."""

        announce("preparing STR dir @ %s" % self.o.testsuite_dir)

        # Produce REST from the tests results dropped by testsuite run.  If we
        # did not launch one just above, expect results to be available from a
        # previous run, possibly remote:

        raccess = raccess_in(self.o.testsuite_dir)
        rdir = rdir_in(self.o.testsuite_dir)

        mkstr_cmd = "(cd %(dir)s/STR && sh ./mkrest.sh %(level)s)" % {
            "dir": self.o.testsuite_dir if not rdir else rdir,
            "level": self.o.dolevel,
        }

        prefix = ["sh", "-c"] if not raccess else ["ssh", raccess]
        run_list(prefix + [mkstr_cmd])

    # ---------------
    # -- build_str --
    # ---------------

    def build_str(self):
        announce("building %s STR" % self.this_docformat)

        str_dir = os.path.join(self.qualdir, "str")

        source_str_dir = os.path.join(
            self.local_testsuite_dir, "STR", "source"
        )
        dest_str_dir = os.path.join(str_dir, "source")

        # Copy the sources from the testsuite into the work dir
        sync_tree(source_str_dir, dest_str_dir, delete=True)

        run("make %s" % self.this_docformat, dirname=str_dir)

        self.__latch_into(
            dirname=self.itemsdir(),
            part="str",
            toplevel=False,
            copy_from=os.path.join(str_dir, "build"),
        )

    # -----------------
    # -- build_plans --
    # -----------------

    def build_plans(self):
        plans_root = os.path.join(
            self.repodir,
            "qualification",
            "qualkit",
            "plans",
        )
        trace_specific_content = [
            os.path.join(
                plans_root, "Tool_Qualification_Plan", "Qualified_Interface"
            )
        ]
        self.prepare_content(trace_specific_content, root=plans_root)

        run(f"make {self.this_docformat}", dirname=plans_root)

        self.__latch_into(
            dirname=self.itemsdir(),
            part="plans",
            toplevel=False,
            copy_from=os.path.join(plans_root, "build"),
        )

    # ---------------
    # -- build_kit --
    # ---------------

    def __relocate_into(self, dirname, part):
        the_item = self.kititem_for(part=part)

        item_source_path = os.path.join(self.itemsdir(), the_item)
        item_target_path = os.path.join(dirname, the_item)

        remove(item_target_path)

        print("move : %s" % item_source_path)
        print("into : %s" % dirname)

        mv(item_source_path, dirname)

    def build_kit(self):
        announce("building %s kit" % self.this_docformat)

        os.chdir(self.workdir)

        # The kit name is computed as:
        #
        #    gnatcov-qualkit-<kitid>-<YYYYMMDD>
        #
        # <YYYYMMDD> is the kit production stamp (now),
        # <kitid> is the kit identifier, typically <cust#>-<project#>.

        today = date.today()

        # If the kitid is not provided, assume the artifacts repo image holds
        # a git clone and compute the id from the current branch there:

        if self.o.kitid:
            kitid = self.o.kitid
        else:
            gitbranch = current_gitbranch_at(self.repodir)

            kitid = gitbranch.replace("qualkit-", "")
            kitid = kitid.replace("/", "-")
            kitid = kitid.replace(".", "_")

        # If we are re-constructing a kit with some parts just rebuilt, target
        # the specified version (stamp) and arrange to keep the old elements
        # in place:

        kitstamp = (
            self.o.rekit
            if self.o.rekit
            else "%4d%02d%02d" % (today.year, today.month, today.day)
        )
        kitname = "gnatcov-qualkit-%s-%s" % (kitid, kitstamp)
        kitdir = "%s-%s" % (kitname, self.this_docformat)

        mkdir(kitdir)

        for part in self.o.parts:
            self.__relocate_into(dirname=kitdir, part=part)

        run("zip -q -r %(kitdir)s.zip %(kitdir)s" % {"kitdir": kitdir})

    # -----------------------
    # -- zip_testsuite_dir --
    # -----------------------

    def zip_testsuite_dir(self):
        os.chdir(self.workdir)

        relative_testsuite_dir = os.path.basename(self.local_testsuite_dir)

        zipname = "%s.zip" % relative_testsuite_dir

        remove(zipname)
        run("zip -q -r %s %s" % (zipname, relative_testsuite_dir))

    # ---------------------
    # -- build_as_needed --
    # ---------------------

    def do_str(self):
        return "str" in self.o.parts

    def do_tor(self):
        return "tor" in self.o.parts

    def do_plans(self):
        return "plans" in self.o.parts

    def do_kit(self):
        return self.o.kitp

    def build_as_needed(self, docformat):
        self.passno += 1
        self.this_docformat = docformat

        mkdir(self.itemsdir())

        if self.do_str() and self.passno == 1:
            self.__prepare_str_dir()

        if self.o.testsuite_dir and not self.local_testsuite_dir:
            self.__localize_testsuite_dir()

        if not self.settings:
            self.load_settings()

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

valid_docformats = ("html", "pdf")
valid_parts = ("tor", "plans", "str")
valid_dolevels = ("doA", "doB", "doC")
valid_xada = ("95", "2005", "2012")
valid_languages = ["Ada%s" % version for version in valid_xada]
valid_trace_modes = ("src", "bin")


def commandline():
    """Build and return an OptionParser instance for this script."""

    op = optparse.OptionParser(usage="%prog <options>")

    op.add_option(
        "--work-dir",
        dest="workdir",
        help=(
            "Name of a directory from where a previous kit construction "
            "will resume. Must exist already."
        ),
    )

    op.add_option(
        "--git-source",
        dest="gitsource",
        default=None,
        help=(
            "Git repo we should be cloning to get our source artifacts. "
            "!! This overrides whatever is in a --work-dir already !!"
        ),
    )
    op.add_option(
        "--git-pull",
        dest="gitpull",
        action="store_true",
        default=False,
        help=(
            "Pull commits from current origin in the git clone setup "
            "in work-dir."
        ),
    )
    op.add_option(
        "--git-reuse",
        dest="gitreuse",
        action="store_true",
        default=False,
        help=("Reuse current git clone setup in work-dir, as-is. "),
    )
    op.add_option(
        "--rsync-from",
        dest="rsync_from",
        default=False,
        help=("Rsync an existing repo image into our local image dir."),
    )
    op.add_option(
        "--branch",
        dest="branchname",
        default=None,
        help=("The git branch we shall produce the material from."),
    )
    op.add_option(
        "--kitid",
        dest="kitid",
        default=False,
        help=("Use the provided argument as the kit identifier."),
    )

    op.add_option(
        "--docformat",
        dest="docformat",
        type="string",  # choices=valid_docformats,
        help=(
            "The format we need to produce for each document. "
            "At least one of %s. Multiple values separated by ','."
            % valid_docformats.__str__()
        ),
    )
    op.add_option(
        "--parts",
        dest="parts",
        default=None,
        help=(
            "A comma separated list of the parts of the qualkit that "
            "are to be generated, subset of %s." % valid_parts.__str__()
        ),
    )

    op.add_option(
        "--dolevel",
        dest="dolevel",
        default=None,
        type="choice",
        choices=valid_dolevels,
        help=(
            "Target DO178 qualification level. One of %s."
            % valid_dolevels.__str__()
        ),
    )

    op.add_option(
        "--languages",
        dest="languages",
        default=None,
        help=(
            "Comma separated list of languages, for traceability matrix "
            "purposes. Amongst %s." % valid_languages.__str__()
        ),
    )

    op.add_option(
        "--testsuite-dir",
        dest="testsuite_dir",
        default=None,
        help=("Name of a directory where the testsuite was run"),
    )

    op.add_option(
        "--rekit",
        dest="rekit",
        default=None,
        help=(
            "rebuild the specified --parts and re-package them in the "
            "kit stamped according to this option's value (YYYYMMDD)"
        ),
    )

    op.add_option(
        "--xgnatpro",
        dest="xgnatpro",
        default=None,
        help=(
            "Version we expect <target>-gcc -v to match. "
            "Mandatory when producing a kit"
        ),
    )
    op.add_option(
        "--xgnatcov",
        dest="xgnatcov",
        default=None,
        help=(
            "Version we expect gnatcov --version to match. "
            "Mandatory when producing a kit"
        ),
    )
    op.add_option(
        "--xgnatemu",
        dest="xgnatemu",
        default=None,
        help=("Version we expect <target>-gnatcov --version to match."),
    )

    op.add_option(
        "--xada",
        dest="xada",
        default=None,
        choices=valid_xada,
        help=(
            "Version of the Ada language we are qualifying for (%s)"
            % "|".join(valid_xada)
        ),
    )

    op.add_option(
        "--trace-mode",
        dest="trace_mode",
        default=None,
        choices=valid_trace_modes,
        help="Trace kind we are qualifying gnatcov for. One of 'src' or 'bin'",
    )

    return op


def check_valid(options, args):
    # We are producing qualification material. Better know what we're
    # aiming at, always:

    exit_if(
        not options.dolevel, "Please specify an explicit dolevel (--dolevel)."
    )

    exit_if(
        not options.trace_mode,
        "Please specify an explicit trace-mode (--trace-mode).",
    )

    # Generating docs can be pretty long. Better make sure the output format
    # was intentionally stated:

    exit_if(
        not options.docformat,
        ("Please specify the desired output format (--docformat)."),
    )

    # Convey whether we are requested to produce a kit:

    options.kitp = options.rekit or not options.parts

    # Settle on the set of documents we are to produce:

    options.parts = (
        valid_parts if not options.parts else options.parts.split(",")
    )

    [
        exit_if(
            part not in valid_parts,
            "Requested part '%s' is invalid, none of %s"
            % (part, valid_parts.__str__()),
        )
        for part in options.parts
    ]

    # work dir

    exit_if(not options.workdir, "A work dir must be specified (--work-dir)")

    warn_if(
        options.kitp
        and os.path.exists(options.workdir)
        and ls("%s/*" % options.workdir),
        "producing kit within non empty workdir",
    )

    # Producing a STR requires a testsuite dir

    exit_if(
        "str" in options.parts and not options.testsuite_dir,
        "--testsuite-dir required when producing a STR",
    )

    # GIT aspects:

    exit_if(
        options.gitpull and options.gitsource,
        "Specifying git source is incompatible with "
        "request to pull from current origin",
    )

    # In case we produce TOR/LRM traceability matrixes ...

    exit_if(
        not options.languages,
        "Please specify the qualified languages (--languages)",
    )

    # -xada, -xgnatcov etc

    # ??? We need access to the suite context data for these checks.
    # We don't always have it handy and fixing this requires moving to
    # a dump format amenable to data exchange across hosts and python
    # versions. We use pickle today, which doesn't fit the bill.


if __name__ == "__main__":
    (options, args) = commandline().parse_args()

    check_valid(options, args)

    # Make sure that directory options are absolute dirs, to
    # facilitate switching back and forth from one to the other:

    if options.testsuite_dir and not raccess_in(options.testsuite_dir):
        options.testsuite_dir = os.path.abspath(options.testsuite_dir)

    # Instanciate our helper and proceed with the base directory setup:

    qmat = QMAT(options=options)

    qmat.setup_workdir()

    if options.rsync_from:
        qmat.rsync_from(options.rsync_from)
    else:
        qmat.git_update()

    if options.branchname:
        qmat.switch_to_branch()

    # Build the various parts and maybe the kit for each requested format:

    [qmat.build_as_needed(docformat=f) for f in options.docformat.split(",")]

    if options.kitp:
        qmat.zip_testsuite_dir()
        qmat.dump_kit_consistency_log()
