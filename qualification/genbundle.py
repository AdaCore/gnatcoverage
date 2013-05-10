#!python

from gnatpython.ex import Run
from gnatpython.fileutils import cp, mv, rm, mkdir

from datetime import date

import optparse, sys, os.path, shutil

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

def run_list (cmd, out=None, env=None):
    print "from : %s" % os.getcwd()
    print "run  : %s" % ' '.join(cmd)

    if out == None:
        out = cmd[0]+".log"

    p = Run (cmd, output=out, env=env)

    fail_if (
        p.status != 0, "execution failed\n"
        + "log was:\n" + contents_of(out))

def run (s, out=None, env=None):
    run_list (s.split())

def announce (s):
    print "=========== " + s

def remove (path):
    """Delete the file or directory subtree designated by PATH"""

    # To prevent big damage if the input PATH happens to have been
    # miscomputed, we first attempt to move it locally, then remove the local
    # instance. The absence of computation on this local name makes it a tad
    # safer to manipulate.

    local_name = "./old_stuff_to_be_removed"

    # Start by removing the current local instance, in case the
    # previous removal failed or was interrupted somehow

    rm (local_name, recursive=True)

    if os.path.exists (path):
        mv (path, local_name)
        rm (local_name, recursive=True)

# =======================================================================
# ==              QUALIF MATERIAL GENERATION HELPER CLASS              ==
# =======================================================================

sphinx_target_for = {
    "html": "html",
    "pdf" : "latexpdf"
    }

# The master GIT repo we will be cloning to get our artifacts
GIT_MASTER = "ssh://git.eu.adacore.com/scmrepos/git/gnatcoverage"

# The subdir name for this clone, relative to --root
GIT_CLONE_SUBDIR = "gnatcoverage-git-clone"

class QMAT:

    def __init__(self, options):

        self.o = options

        self.rootdir =  os.path.abspath (options.rootdir)
        self.itemsdir = os.path.join (self.rootdir, "ITEMS")

        self.repodir = os.path.join (self.rootdir, GIT_CLONE_SUBDIR)

    # --------------------
    # -- setup_basedirs --
    # --------------------

    def setup_basedirs (self):

        announce ("setting up working dirs from %s" % self.rootdir)

        mkdir (self.rootdir)
        mkdir (self.itemsdir)


    # -----------------------
    # -- clone_master_repo --
    # -----------------------

    def clone_master_repo (self):
        announce ("cloning master git repository")

        os.chdir(self.rootdir)
        run ("git clone %s %s" % (GIT_MASTER, GIT_CLONE_SUBDIR))

    # ----------------------
    # -- switch_to_branch --
    # ----------------------

    def switch_to_branch (self):
        announce ("switching to branch '%s'" % self.o.branchname)

        os.chdir(self.repodir)
        run ("git checkout %s" % self.o.branchname)

    # ----------------
    # -- latch_part --
    # ----------------

    # Helper for the various build_ methods below.

    # html builds are voluminous and tree-ish. Other builds might produce
    # secondary pieces we don't need (e.g. latex sources & stuff) and we
    # only care about the final file at the end.

    # For tree builds, we just rename the whole sphinx build tree as our
    # result. For other builds, we use a wildcard copy so the actual file
    # name doesn't matter:

    def __latch_into (self, dir, partname, toplevel):

        this_target_is_tree = (self.o.docformat == 'html')

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

        this_build_subdir = os.path.join (
            "build", sphinx_target_for[self.o.docformat])

        # Delete an old version of latched results that might
        # already be there if we're running with --reuse.
        remove (this_target)

        if this_target_is_tree:
            mv (this_build_subdir,
                this_target)
        else:
            cp (this_build_subdir + "/*.%s" % self.o.docformat,
                this_target)

        print "%s %s available in %s %s" % (
            self.o.docformat, partname,
            this_target, "(toplevel)" if toplevel else ""
            )

    # ---------------
    # -- build_tor --
    # ---------------

    def build_tor (self):
        announce ("building TOR")

        # Building the TOR documents involves generating REST
        # from the artifacts in the testsuite/Qualif tree, then
        # invoking sphinx to produce the document. This is all
        # driven by a Makefile:

        os.chdir (
            os.path.join (
                self.repodir, "qualification", "tor", "scripts")
            )

        make_vars = (
            "CHAPTERS='%s'" % self.o.re_chapters if self.o.re_chapters else ""
            )

        run ("make %(vars)s clean genrest" % {"vars": make_vars})

        run ("make %(vars)s %(fmt)s " % {
                "vars": make_vars,
                "fmt" : sphinx_target_for[self.o.docformat]}
             )

        self.__latch_into (
            dir=self.itemsdir, partname="TOR", toplevel=False)

    # ---------------
    # -- build_str --
    # ---------------

    def build_str (self):
        announce ("building STR")

        # Building the STR document first involves running the testsuite in
        # qualif mode (--qualif-level), producing REST from results dropped by
        # each testcase execution:

        os.chdir (os.path.join (self.repodir, "testsuite"))

        if not os.path.exists ("support"):
            orisupport = os.path.join (
                "..", "tools", "gnatcov", "examples", "support")
            if os.path.exists (orisupport):
                shutil.move (orisupport, "support")

        base_cmd = (
            "python testsuite.py "
            "--target=ppc-elf --RTS=powerpc-elf/zfp-prep "
            "--qualif-level=%s -j4" % self.o.dolevel
            )

        all_cargs = []
        if self.o.cargs:
            all_cargs.append ('--cargs=%s' % self.o.cargs)
        if self.o.cargs_ada:
            all_cargs.append ('--cargs:Ada="%s"' % self.o.cargs_ada)

        re_tests_args = (
            [] if self.o.re_tests is None else [self.o.re_tests])

        run_list (
            base_cmd.split() + all_cargs + re_tests_args
            )

        # Then resort to sphinx to produce the document from REST, in the
        # requested output format:

        os.chdir (os.path.join (self.repodir, "testsuite", "qreport"))
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
valid_parts      = ('tor', 'str', 'plans')
valid_dolevels   = ('doA', 'doB', 'doC')

if __name__ == "__main__":

    op = optparse.OptionParser(usage="%prog <options>")
    op.add_option (
        "--root-dir", dest="rootdir",
        help=(
            "Name of a directory where the kit construction will take place. "
            "Must not exist already.")
        )
    op.add_option (
        "--package-name", dest="pname",
        help=(
            "Base name of the .zip archive that will contain the full set of "
            "items bundled together. Ignored if the set of constructed items "
            "is specified explicitly.")
        )
    op.add_option ("-t", "--re_tests", dest="re_tests")
    op.add_option ("-c", "--re_chapters", dest="re_chapters")

    op.add_option (
        "--reuse", dest="reuse", action="store_true", default=False,
        help = (
            "Reuse the provided root dir.")
        )
    op.add_option (
        "--docformat", dest="docformat", default="html",
        type='choice', choices=valid_docformats,
        help = (
            "The format we need to produce for each document %s."
            "One of %s." % (valid_parts.__str__(), valid_docformats.__str__()))
        )
    op.add_option (
        "--parts", dest="parts", default=None,
        help = (
            "A comma separated list of the parts of the qualkit that "
            "are to be generated, subset of %s." % valid_parts.__str__())
        )
    op.add_option (
        "--branch", dest="branchname", default="opendo",
        help = (
            "The git branch we shall produce the material from.")
        )
    op.add_option (
        "--dolevel", dest="dolevel", default=None,
        type='choice', choices=valid_dolevels,
        help = (
            "Target DO178 qualification level. One of %s." \
                % valid_dolevels.__str__())
        )
    op.add_option (
        "--cargs", dest="cargs",
        help = (
            "Language agnostic compilation flags (-O0, -O1, ...)")
        )
    op.add_option (
        "--cargs:Ada", dest="cargs_ada",
        help = (
            "Ada specific compilation flags (-gnatp, -gnatn, ...)")
        )

    (options, args) = op.parse_args()

    exit_if (
        not options.rootdir,
        "A root work dir must be specified (--root)"
        )

    exit_if (
        not options.reuse and os.path.exists (options.rootdir),
        "Without --reuse, the --root dir (%s) must not exist already" \
            % options.rootdir
        )

    exit_if (
        options.pname and options.parts and not options.reuse,
        ("No archive (--pname) may be generated with "
         "only parts of the kit (--parts).")
        )

    # If we are generating a full kit, we need to produce an archive.
    # Pick a default name if none was specified:

    if not options.parts and not options.pname:
        today = date.today()
        options.pname = "GNATCOV-QMAT-%s-%4d-%02d-%02d" % (
            options.docformat.upper(), today.year, today.month, today.day)

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

    exit_if (
        'str' in options.parts and not options.dolevel,
        ("Producing STR requires an explicit dolevel (--dolevel).")
        )

    qmat = QMAT (options=options)

    qmat.setup_basedirs()

    if not options.reuse:
        qmat.clone_master_repo()

    qmat.switch_to_branch()

    # Produce each part we are requested to produce:

    if 'tor' in options.parts:
        qmat.build_tor()

    if 'str' in options.parts:
        qmat.build_str()

    if 'plans' in options.parts:
        qmat.build_plans()

    # If we have a package to produce, do so:

    if options.pname:
        qmat.build_pack()
