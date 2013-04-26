#!python

from gnatpython.ex import Run
from gnatpython.fileutils import cp, mv

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
GIT_CLONE_SUBDIR = "gnatcoverage"

class QMAT:

    def __init__(self, options):

        self.docformat = options.docformat
        self.pname = options.pname

        self.re_tests = options.re_tests
        self.re_chapters = options.re_chapters
        self.dolevel = options.dolevel

        self.rootdir =  os.path.abspath (options.rootdir)
        self.itemsdir = os.path.join (self.rootdir, "ITEMS")

        self.repodir = os.path.join (self.rootdir, GIT_CLONE_SUBDIR)
        self.branchname = options.branchname

    def setup_basedirs (self):

        announce ("setting up working dirs from %s" % self.rootdir)

        os.mkdir (self.rootdir)
        fail_if (
            not os.path.isdir(self.rootdir),
            "creation of root dir '%s' failed somehow" % self.rootdir
            )

        os.mkdir (self.itemsdir)


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
        announce ("switching to branch '%s'" % self.branchname)

        os.chdir(self.repodir)
        run ("git checkout %s" % self.branchname)

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
            "CHAPTERS='%s'" % self.re_chapters if self.re_chapters else ""
            )

        sphinx_target = sphinx_target_for[self.docformat]

        run ("make %(vars)s clean genrest %(fmt)s " % {
                "vars": make_vars,
                "fmt" : sphinx_target }
             )

        # html builds are voluminous and tree-ish. Other builds might produce
        # secondary pieces we don't need (e.g. latex sources & stuff) and we
        # only care about the final file at the end.

        # For tree builds, we just rename the whole sphinx build tree as our
        # result. For other builds, we use a wildcard copy so the actual file
        # name doesn't matter:

        this_target_suffix = (
            'tree' if self.docformat == 'html' else ''
            )
        this_target = os.path.join (
            self.itemsdir, "TOR.%(fmt)s%(suffix)s" % {
                "fmt": self.docformat,
                "suffix": this_target_suffix }
            )
        this_build_subdir = os.path.join ("build", sphinx_target)

        if this_target_suffix == 'tree':
            mv (this_build_subdir,
                this_target)
        else:
            cp (this_build_subdir + "/*.%s" % self.docformat,
                this_target)

        print "%s TOR availalable in %s" % (self.docformat, this_target)

    def build_str (self):
        announce ("building STR")

        os.chdir (os.path.join (self.repodir, "testsuite"))

        orisupport = os.path.join (
            "..", "tools", "gnatcov", "examples", "support")

        if os.path.exists (orisupport):
            shutil.move (orisupport, "support")

        base_cmd = (
            "python testsuite.py "
            "--target=ppc-elf --RTS=powerpc-elf/zfp-prep "
            "--qualif-level=%s -j4" % self.dolevel
            )

        run_list (
            base_cmd.split() + [
                '--qualif-cargs=-O0',
                '--qualif-cargs-Ada=-gnatp',
                self.re_tests ]
            )

        # ??? How would we go about passing multiple options in a single
        # qualif-cargs here ? quotes get through, as part of the option text,
        # as well ...

        os.chdir (os.path.join (self.repodir, "testsuite", "qreport"))
        run ("make html")

        shutil.move (
            os.path.join ("build", "html"),
            os.path.join (self.itemsdir, "STR"))

    def build_plans (self):
        announce ("building PLANS")

        if self.options.use_qm:
            os.chdir (
                os.path.join (self.repodir, "qualification", "qm"))
            run ("qm_server -l scripts/generate_plan.py -p 0 .")

            shutil.move (
                os.path.join (
                    self.repodir, "qualification", "qm", "plans", "html"),
                os.path.join (self.itemsdir, "PLANS"))

        else:
            os.chdir (os.path.join (self.repodir, "qualification", "plans"))
            run ("tar xzf html.tar.gz")

            shutil.move (
                os.path.join (self.repodir, "qualification", "plans", "html"),
                os.path.join (self.itemsdir, "PLANS"))

    def build_pack (self):
        announce ("building INDEX")

        os.chdir (os.path.join (self.repodir, "qualification", "index"))
        run ("make html")

        packroot = os.path.join (self.rootdir, self.pname)

        fail_if (
            os.path.exists (packroot), "%s exists already !!" % packroot
            )

        shutil.move (os.path.join ("build", "html"), packroot)
        shutil.move (self.itemsdir, packroot)

        os.chdir (self.rootdir)

        run ("zip -q -r %(packname)s.zip %(packname)s" % {
                "packname": self.pname})

# =======================================================================
# ==                          MAIN SCRIPT BODY                         ==
# =======================================================================

valid_docformats = ('html', 'pdf')
valid_parts      = ('tor', 'str', 'plans')
valid_dolevels   = ('doA', 'doB', 'doC')

if __name__ == "__main__":

    op = optparse.OptionParser(usage="%prog <options>")
    op.add_option (
        "-r", "--root-dir", dest="rootdir",
        help=(
            "Name of a directory where the kit construction will take place. "
            "Must not exist already.")
        )
    op.add_option (
        "-o", "--package-name", dest="pname",
        help=(
            "Base name of the .zip archive that will contain the full set of "
            "items bundled together. Ignored if the set of constructed items "
            "is specified explicitly.")
        )
    op.add_option ("-t", "--re_tests", dest="re_tests")
    op.add_option ("-c", "--re_chapters", dest="re_chapters")

    op.add_option (
        "-u", "--reuse", dest="reuse", action="store_true", default=False,
        help = (
            "Reuse the provided root dir.")
        )
    op.add_option (
        "-q", "--use-qm", dest="use_qm", action="store_true", default=False,
        help = (
            "Whether we should use the QM to build the QM managed documents." 
            "Fetch a static version from the SCM tree otherwise.")
        )
    op.add_option (
        "-f", "--docformat", dest="docformat", default="html",
        type='choice', choices=valid_docformats,
        help = (
            "The format we need to produce for each document %s."
            "One of %s." % (valid_parts.__str__(), valid_docformats.__str__()))
        )
    op.add_option (
        "-p", "--parts", dest="parts", default=None,
        type='choice', choices=valid_parts,
        help = (
            "A comma separated list of the parts of the qualkit that "
            "are to be generated, subset of %s." % valid_parts.__str__())
        )
    op.add_option (
        "-b", "--branch", dest="branchname", default="opendo",
        help = (
            "The git branch we shall produce the material from.")
        )
    op.add_option (
        "-d", "--dolevel", dest="dolevel",
        type='choice', choices=valid_dolevels,
        help = (
            "Target DO178 qualification level. One of %s." \
                % valid_dolevels.__str__())
        )

    (options, args) = op.parse_args()

    exit_if (
        not options.rootdir,
        "A root work dir must be specified (-r)"
        )

    exit_if (
        not options.reuse and os.path.exists (options.rootdir),
        "Without --reuse, the --root dir (%s) must not exist already" \
            % options.rootdir
        )

    exit_if (
        options.pname and options.parts,
        "No archive (-o) may be generated with only parts of the kit (-p)"
        )

    # If we are generating a full kit, we need to produce an archive.
    # Pick a default name if none was specified:

    if not options.parts and not options.pname:
        today = date.today()
        options.pname = "GNATCOV-QMAT-%4d-%02d-%02d" % (
            today.year, today.month, today.day)

    # Settle on the set of documents we are to produce:

    options.parts = (
        () if not options.parts
        else options.parts.split(',')
        )

    [exit_if (
            part not in valid_parts,
            "Requested part '%s' is invalid, none of %s" \
                % (part, valid_parts.__str__())
            )
     for part in options.parts]
            
    if options.re_tests == None:
        options.re_tests = "Qualif/(Ada|Common)"

    qmat = QMAT (options=options)

    # Unless we are instructed to reuse the provided root dir,
    # set it up:

    if not options.reuse:
        qmat.setup_basedirs()
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

