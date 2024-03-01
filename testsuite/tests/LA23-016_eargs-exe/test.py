from SUITE.context import thistest
from SUITE.cutils import Wdir, empty, contents_of, match
from SUITE.tutils import exepath_to, gprbuild, gprfor, xcov, xrun


Wdir("tmp_")

gprbuild(gprfor(srcdirs=["../src"], mains=["p.adb"]))
exe = exepath_to("p")


# Check that "xcov run p" and "xcov run -eargs p"
# both work and produce the same result.


def run_check(slug, args):
    rlog = slug + ".rlog"
    trace = slug + ".trace"
    xrun(["-o", trace] + args, out=rlog)
    thistest.fail_if(
        not empty(rlog) and not match("/runkernel", rlog),
        "unexpected output from %s run:\n%s" % (slug, contents_of(rlog)),
    )


def cov_check(slug):
    clog = slug + ".clog"
    trace = slug + ".trace"
    rep = slug + ".rep"
    xcov(
        [
            "coverage",
            "--level=stmt",
            "--scos=obj/myabs.ali",
            "--annotate=report",
            trace,
            "-o",
            rep,
        ],
        out=clog,
    )

    thistest.fail_if(
        not match("statement not executed", rep),
        "missing expected stmt coverage violation note in %s" % rep,
    )

    thistest.fail_if(
        not empty(clog),
        "unexpected output from %s coverage :\n%s" % (slug, contents_of(clog)),
    )


slug = "base"
run_check(slug, args=[exe])
cov_check(slug)

slug = "eargs"
run_check(slug, args=["-eargs", exe])
cov_check(slug)

# Check that a bare "xcov run" complains about absence of executable
# on the command line.
log = "noexe.log"
xrun("", out=log, register_failure=False)

thistest.fail_if(
    not match(": Please specify an executable to run", log),
    "missing expected error in log for run without exe",
)

thistest.result()
