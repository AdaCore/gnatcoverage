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
# 2) build whatever is requested (plans, str, tor) from those artifacts,
#    producing the html or pdf document of interest + stuff we don't care
#    about (e.g. intermediate latex sources for rest->pdf),
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
#   python genbundle.py
#     --root-dir=$HOME/my-qmat
#     --git-source=$HOME/gnatcoverage --branch=dev-str
#     --parts=plans
#
# Testing plans regeneration after local commit:
#
#   python genbundle.py
#      --work-dir=$HOME/my-qmat
#      --git-pull --branch=dev-str
#      --parts=plans
#
# Testing STR production, running a subset of the tests, using
# the local testsuite and compiler on path:
#
#   python genbundle.py
#     --work-dir=$HOME/my-qmat
#     --git-pull --branch=dev-str
#     --parts=str
#     --runtests --runtests-flags="--target... --RTS... stmt/Robustness -j4"
#     --dolevel=doB
#
# This example uses --runtests without specifying a testsuite-dir, so
# the testsuite run takes place within the cloned tree in $HOME/my-qmat.
#
# Testing another STR production after local commits, using previous testsuite
# results at a designated place:
#
#   python genbundle.py
#     --work-dir=$HOME/my-qmat
#     --git-pull --branch=dev-str
#     --parts=str
#     --testsuite-dir=$HOME/gnatcoverage/testsuite
#     --dolevel=doB
#
# Note that the designated testsuite dir in this example is NOT the one
# populated by the --runtests example before.
#
# Example kit production commands:
# ================================
#
# Producing a packaged kit, with a toplevel index and a final .zip
# archive is achieved by not restricting to specific parts, hence by
# not passing --parts at all.
#
# A DO level has to be provided in this case, and variations are allowed
# testsuite execution and results localization:
#
# Running the tests from the local clone this script creates:
# -----------------------------------------------------------
#
#   python genbundle.py
#     --root-dir=$HOME/my-qmat
#     --branch=<project-branch>
#     --dolevel=doA
#     --runtests --runtests-flags=<...>
#
# Running the tests hosted within the designated subdir:
# ------------------------------------------------------
#
#   python genbundle.py
#     --root-dir=$HOME/my-qmat
#     --branch=<project-branch>
#     --dolevel=doB
#     --runtests --runtests-flags=<...>
#     --testsuite-dir=<...>
#
# Fetching tests results from a place where they have been run already:
# ---------------------------------------------------------------------
#
#   python genbundle.py
#     --root-dir=$HOME/my-qmat
#     --branch=<project-branch>
#     --dolevel=doA
#     --testsuite-dir=<...>
#
# !!! As of today, this is reading data files dumped by the testsuite
# !!! execution in pickle format. This format is not designed to be shared
# !!! across different environments, so isn't guaranteed to work when e.g. the
# !!! suite ran on one machine and we're reading data from another. Even on a
# !!! single machine, this isn't guaranteed to work across python versions.
#
# We provide the 'str-rst' part and the --str-dir option to circumvent this:
#
# Using a partially built (up to rst generation) STR report:
# ----------------------------------------------------------
#
# The STR rest production must be done first, using the special "str-rst"
# part on the machine where the testsuite ran, using the same version of
# python. For example:
#
#   python genbundle.py
#     --root-dir=$HOME/my-partial-qmat
#     --branch=<project-branch>
#     --dolevel=doA
#     --testsuite-dir=<my-testsuite-dir>
#     --parts=str-rst
#
# This should output something like:
#
#   =========== building STR, stopping after rst gen
#   from : <...>/gnatcoverage-git-clone/qualification/str
#   run  : python genrest.py --testsuite-dir=<my-testsuite-dir> --dolevel=doB
#
# Now on the machine where you are building the kit, retrieve the
# rest-only STR subdir. For example using rsync:
#
#   rsync -r <testhost>: <...>/gnatcoverage-git-clone/qualification/str/ my-str
#
# Then produce your kit using the retrieved STR subdir:
#
#    python genbundle.py
#       --root-dir=$HOME/my-qmat
#       --branch=<project-branch>
#       --dolevel=doB
#       --str-dir=$HOME/my-str
#
# *****************************************************************************

from gnatpython.ex import Run
from gnatpython.fileutils import cp, mv, rm, mkdir

from datetime import date

import optparse, sys, os.path, shutil, re

LOCAL_TESTSUITE_DIR=os.path.abspath("../testsuite")
sys.path.append(LOCAL_TESTSUITE_DIR)

from SUITE.qdata import CTXDATA_FILE, treeref_at
from SUITE.cutils import load_from

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

def contents_of(filename):
    with open(filename) as fd:
        return fd.read()

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

    out = cmd[0]+".log"
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

    def __init__(self, options):

        self.o = options

        self.rootdir =  os.path.abspath (
            options.rootdir if options.rootdir else options.workdir)
        self.itemsdir = os.path.join (self.rootdir, "ITEMS")

        self.repodir = os.path.join (self.rootdir, GIT_CLONE_SUBDIR)

        # Where the testsuite tree is to be found, to run the
        # tests if needed, then to fetch results from (whether we
        # run the tests ourselves or rely on a prior run)

        self.testsuite_dir = (
            self.o.testsuite_dir if self.o.testsuite_dir
            else os.path.join (self.repodir, "testsuite") if self.o.runtests
            else None)

    # --------------------
    # -- setup_basedirs --
    # --------------------

    def setup_basedirs (self):

        announce ("setting up working dirs from %s" % self.rootdir)

        mkdir (self.rootdir)
        mkdir (self.itemsdir)


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

    # ----------------
    # -- latch_into --
    # ----------------

    # Helper for the various build_ methods below, to store the just built
    # sphinx output for a provided PARTNAME (STR, PLANS, TOR) at a TOPLEVEL
    # dir.

    # html builds are voluminous and tree-ish. Other builds might produce
    # secondary pieces we don't need (e.g. latex sources & stuff) and we
    # only care about the final file at the end.

    # For tree builds, we just rename the whole sphinx build tree as our
    # result. For example:
    #
    # build/html for PLANS gets renamed as <TOPLEVEL>/PLANS, a subdirectory
    # where we expect index.html to be found.

    # For other builds, we use a wildcard copy so the actual file
    # name doesn't matter. For example:
    #
    # build/pdf/TOR.pdf gets copied as <TOPLEVEL>/TOR.pdf

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

    def __latch_into (self, dir, partname, toplevel, copy_from=None):

        this_target_is_tree = (self.o.docformat == 'html')

        # Compute the target dir or file name of our copy:

        this_target_suffix = (
            '' if this_target_is_tree else '.%s' % self.o.docformat)

        this_target = (
            dir if toplevel and this_target_is_tree
            else os.path.join (
                dir, "%(part)s%(suffix)s" % {
                    "part": partname,
                    "suffix": this_target_suffix }
                )
            )

        # Compute the source dir or file name for our copy:

        # The local or provided source build subdir name, assuming a
        # sphinx setup, with an html or pdf sub-subdir depending on the
        # doc format:

        this_build_subdir = os.path.join (
            copy_from if copy_from is not None else "build",
            sphinx_target_for[self.o.docformat]
            )

        # We used to use a wildcard for the source name, but that didn't
        # work because two different pdf files are generated in the same
        # directory.  The following approach is "ugly", but also intended
        # to be temporary!
        if partname == 'TOR':
            source_name = 'TOR_Doc'
        elif partname == 'PLANS':
            source_name = 'plans'
        elif partname == 'STR':
            source_name = 'GNATcoverage'
        elif 'GNATCOV-QMAT-PDF' in partname:
            source_name = 'GNATcoverageQualificationMaterial'
        else:
            source_name = partname

        this_source = (
            this_build_subdir if this_target_is_tree
            else os.path.join(this_build_subdir,
            source_name + ".%s" % self.o.docformat)
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
            self.o.docformat, partname,
            this_target, "(toplevel)" if toplevel else ""
            )

    # ---------------
    # -- build_tor --
    # ---------------

    def build_tor (self):
        announce ("building TOR")

        # The TORs are managed as QM data

        os.chdir (
            os.path.join (self.repodir, "qualification", "qm")
            )
        run ("qmachine model.xml -l scripts/generate_tor_%s.py" \
                 % self.o.docformat)

        self.__latch_into (
            dir=self.itemsdir, partname="TOR", toplevel=False)

    # ---------------
    # -- run_tests --
    # ---------------

    def run_tests (self):
        announce ("running tests")

        os.chdir (self.testsuite_dir)

        # Setup the testsuite "support" directory, then launch the toplevel
        # suite driver with the requested set of flags and in qualif mode for
        # the level we ought to satisfy:

        if not os.path.exists ("support"):
            orisupport = os.path.join (
                "..", "tools", "gnatcov", "examples", "support")
            if os.path.exists (orisupport):
                cp (source=orisupport, target="support", recursive=True)

        run_list (
            ("python testsuite.py --qualif-level=%s " % self.o.dolevel
             + self.o.runtests_flags).strip().split()
            )

    # ---------------------------
    # -- do_consistency_checks --
    # ---------------------------

    def do_consistency_checks (self):
        announce ("tree and version consistency check prior to STR production")

        # Check consistency of the testsuite tree ref against the clone
        # from which we are producing documents. Check that the latter dir
        # ref matches that of ...
        #
        # * The testsuite dir where we are going to run the tests when
        #   requested so,
        #
        # * The testsuite dir where the tests were run otherwise

        # Also check consistency of the tool versions used to execute the
        # testsuite against expectations

        local_treeref = treeref_at(self.repodir)

        suite_treeref = None
        suite_ctxdata = None

        if self.o.runtests:
            suite_treeref = treeref_at(self.testsuite_dir)

        elif self.o.testsuite_dir:
            suite_ctxdata = load_from (
                os.path.join (self.o.testsuite_dir, CTXDATA_FILE))
            suite_treeref = suite_ctxdata.treeref

        elif self.o.str_dir:
            suite_treeref = None # until we can do better ...
            suite_ctxdata = None

        if not suite_treeref:
            print "info: unable to check tree consistency in this setup"
        else:
            exit_if (
                local_treeref != suite_treeref,
                "local tree ref (%s) mismatches testsuite tree ref (%s)" % (
                    local_treeref, suite_treeref)
                )

        if not suite_ctxdata:
            print "info: unable to check versions consistency in this setup"
        else:
            exit_if (
                self.o.xgnatpro and not re.search (
                    pattern=self.o.xgnatpro,
                    string=suite_ctxdata.gnatpro.version),
                "gnatpro version \"%s\" doesn't match expectation \"%s\"" % (
                    suite_ctxdata.gnatpro.version, self.o.xgnatpro)
                )
            exit_if (
                self.o.xgnatcov and not re.search (
                    pattern=self.o.xgnatcov,
                    string=suite_ctxdata.gnatcov.version),
                "gnatcov version \"%s\" doesn't match expectation \"%s\"" % (
                    suite_ctxdata.gnatpro.version, self.o.xgnatpro)
                )

    # ---------------
    # -- build_str --
    # ---------------

    def build_str (self, rst_only):
        announce (
            "building STR" + (", stopping after rst gen" if rst_only else "")
            )

        # Produce REST from the tests results dropped by testsuite run.  If we
        # did not launch one just above, expect results to be available from a
        # previous run.

        os.chdir (os.path.join (self.repodir, "qualification", "str"))

        run_list (
            ['python', 'genrest.py',
             '--testsuite-dir=%s' % self.testsuite_dir,
             '--dolevel=%s' % self.o.dolevel]
            )

        if rst_only:
            return

        # Then invoke sphinx to produce the report in the requested format:

        run ("make %s" % sphinx_target_for[self.o.docformat])

        self.__latch_into (
            dir=self.itemsdir, partname="STR", toplevel=False)

    # ------------------
    # -- finalize_str --
    # ------------------

    def finalize_str (self):
        announce ("finalizing STR at %s" % self.o.str_dir)

        os.chdir (self.o.str_dir)

        run ("make %s" % sphinx_target_for[self.o.docformat])

        self.__latch_into (
            dir=self.itemsdir, partname="STR", toplevel=False)

    # -----------------
    # -- build_plans --
    # -----------------

    def build_plans (self):
        announce ("building PLANS")

        # The plans are managed as QM data

        os.chdir (
            os.path.join (self.repodir, "qualification", "qm")
            )
        run ("qmachine model.xml -l scripts/generate_plans_%s.py" \
                 % self.o.docformat)

        self.__latch_into (
            dir=self.itemsdir, partname="PLANS", toplevel=False)

    # ----------------
    # -- build_pack --
    # ----------------

    def build_pack (self):
        announce ("building INDEX")

        os.chdir (os.path.join (self.repodir, "qualification", "index"))

        # We have distinct index sources for each docformat, that designate
        # each part with the appropriate location and extension (links pointing
        # to ITEMS/<part>/index.html or to ITEMS/<part>.pdf for example)

        # Rename the one we need and generate our index from there. This will
        # be doing cross document referencing.

        sphinx_target = sphinx_target_for[self.o.docformat]

        cp ("source/index_%s_rst" % self.o.docformat, "source/index.rst")
        run ("make %s" % sphinx_target)

        packroot = os.path.join (self.rootdir, self.o.pname)
        remove (packroot)
        mkdir (packroot)

        self.__latch_into (
            dir=packroot, partname=self.o.pname, toplevel=True)
        shutil.move (self.itemsdir, packroot)

        os.chdir (self.rootdir)

        run ("zip -q -r %(packname)s.zip %(packname)s" % {
                "packname": self.o.pname})

# =======================================================================
# ==                          MAIN SCRIPT BODY                         ==
# =======================================================================

valid_docformats = ('html', 'pdf')
regular_parts    = ('tor', 'plans', 'str')
special_parts    = ('str-rst',)
valid_parts      = regular_parts + special_parts
valid_dolevels   = ('doA', 'doB', 'doC')

if __name__ == "__main__":

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
        "--package-name", dest="pname",
        help=(
            "Base name of the .zip archive that will contain the full set of "
            "items bundled together. Ignored if the set of constructed items "
            "is specified explicitly.")
        )
    op.add_option (
        "--docformat", dest="docformat", default="html",
        type='choice', choices=valid_docformats,
        help = (
            "The format we need to produce for each document. "
            "One of %s." % valid_docformats.__str__())
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
            "Name of a directory where the testsuite was run or is to be "
            "run if --runtests. Defaults to the git clone testsuite subdir.")
        )
    op.add_option (
        "--runtests", dest="runtests", action="store_true", default=None,
        help=(
            "Run the tests prior to fetching results from testsuite-dir."
            "Pass the option value as arguments to the testsuite toplevel "
            "driver, useful e.g. for --target, or -j."
            )
        )
    op.add_option (
        "--runtests-flags", dest="runtests_flags", default="",
        help=(
            "With --runtests, pass the option value as arguments to the "
            "testsuite toplevel driver, useful e.g. for --target, or -j."
            )
        )

    op.add_option (
        "--str-dir", dest="str_dir", default=None,
        help = (
            "Name of a directory where the STR report has been pre-built. "
            "Expect this to designate a sphinx ready subdir, with a Makefile "
            "and a populated source/ sub-sudir.")
        )

    op.add_option (
        "--devmode", dest="devmode", action="store_true", default=False,
        help = (
            "State that we're in ongoing development mode, relaxing internal "
            "consistency checks.")
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

    (options, args) = op.parse_args()

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

    exit_if (
        options.pname and options.parts,
        ("No archive (--pname) may be generated with "
         "only parts of the kit (--parts).")
        )

    # If we are generating a full kit, we need to produce an archive.
    # Pick a default name if none was specified:

    if not options.parts and not options.pname:
        today = date.today()
        options.pname = "GNATCOV-QMAT-%s-%4d-%02d-%02d" % (
            options.docformat.upper(), today.year, today.month, today.day)

    # In principle, we should refuse to generate a package in devmode, as
    # packages are presumably things to be delivered and devmode disconnects
    # consistency checks. In practice, there are often last minute doc
    # adjustments that need to get in and forcing to re-run the tests from
    # the adjusted tree really is unfriendly. --devmode must still be provided
    # explicitly so that assembling pieces from not-quite-consistent trees
    # is acknowledged, with manual verification of the differences for packages
    # to be delivered.

    # exit_if (
    #    options.pname and options.devmode,
    #    "Producing a packaged kit is disallowed in devmode."
    #   )

    # Settle on the set of documents we are to produce:

    options.parts = (
        regular_parts if not options.parts
        else options.parts.split(',')
        )

    [exit_if (
            part not in valid_parts,
            "Requested part '%s' is invalid, none of %s" \
                % (part, valid_parts.__str__())
            )
     for part in options.parts]

    exit_if (
        'str' in options.parts and not options.dolevel,
        ("Producing STR requires an explicit dolevel (--dolevel).")
        )

    exit_if (
        'tor' in options.parts and not options.dolevel,
        ("Producing TOR requires an explicit dolevel (--dolevel).")
        )

    exit_if (
        'str-rst' in options.parts and 'str' in options.parts,
        ("Complete STR is incompatible with rest-only STR.")
        )

    exit_if (
        'str-rst' in options.parts and options.str_dir,
        ("Producing rest-only STR is incompatible with fetching "
         "pre-built STR rest sources.")
        )

    # Make sure that directory options are absolute dirs, to
    # facilitate switching back and forth from one to the other:

    if options.testsuite_dir:
        options.testsuite_dir = os.path.abspath (options.testsuite_dir)
    if options.str_dir:
        options.str_dir = os.path.abspath (options.str_dir)

    # Instanciate our helper and proceed with the base
    # directory setup:

    qmat = QMAT (options=options)

    qmat.setup_basedirs()

    exit_if (
        options.gitpull and options.gitsource,
        "Specifying git source is incompatible with "
        "request to pull from current origin"
        )
    qmat.git_update()

    qmat.switch_to_branch()

    # Produce each part we are requested to produce:

    do_str     = 'str' in options.parts
    do_str_rst = 'str-rst' in options.parts
    do_tor     = 'tor' in options.parts
    do_plans   = 'plans' in options.parts

    if do_tor:
        qmat.build_tor()

    if do_str or do_str_rst:

        # When producing str and not in dev mode, check that the tree we're
        # producing documents from is consistent with the tree where the
        # testsuite has/will run.

        if not options.devmode:
            qmat.do_consistency_checks()

        # If we are provided with pre-computed report sources, just finalize
        # from there:

        if options.str_dir:
            qmat.finalize_str()

        # Otherwise ...

        else:

            # Run the tests if we are requested to do so:

            if options.runtests:
                qmat.run_tests ()

            # Then build the STR from testsuite results (either the one
            # we just ran, or one executed externally and designated by
            # --testsuite-dir):

            qmat.build_str(rst_only=do_str_rst)

    if do_plans:
        qmat.build_plans()

    # If we have a package to produce, do so:

    if options.pname:
        qmat.build_pack()
