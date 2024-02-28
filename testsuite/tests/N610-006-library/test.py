import re

from SCOV.minicheck import build_and_run
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov

Wdir("tmp_")

# We have a library with two units and a test exercising only one
# of them. Check that gnatcov reports the untested stmts as uncovered
# and emits an explicit notice of absence of object code on stderr
# with --verbose.

prj = gprfor(
    ["test_inc.adb"], srcdirs=[".."], objdir="obj", deps=["../libops"]
)

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    mains=["test_inc"],
    extra_coverage_args=[],
)


def trycov(verbose, annotate):
    # last argument of xcov_args it the trace_file name
    # Note that coverage collection is on the library, not on
    # the test project given to build_and_run
    base_cov_command = (
        "coverage --level=stmt --annotate=%s -P../libops.gpr %s"
        % (annotate, xcov_args[-1])
    )

    cov_command = base_cov_command
    if verbose:
        cov_command += " --verbose"

    if annotate != "report":
        cov_command += " --source-search=libops --output-dir=."

    p = xcov(cov_command)

    nocode_notified = re.search(
        pattern="no object code for mult.ali", string=p.out
    )

    bin_mode = thistest.options.trace_mode == "bin"

    if bin_mode and verbose and not nocode_notified:
        thistest.failed(
            "missing notice of absence of object code,"
            " verbose=%s, annotate=%s" % (verbose, annotate)
        )

    if bin_mode and nocode_notified and not verbose:
        thistest.failed(
            "unexpected notice of absence of object code,"
            " verbose=%s, annotate=%s" % (verbose, annotate)
        )

    if annotate == "report":
        thistest.fail_if(
            not re.search(
                pattern="mult.adb.*: statement not executed", string=p.out
            ),
            "missing expected stmt violation for mult.adb in report",
        )

    if annotate == "xcov":
        thistest.fail_if(
            not re.search(
                pattern="[0-9]+ -:", string=contents_of("mult.adb.xcov")
            ),
            "missing expected l- in mult.adb.xcov",
        )


for v in (True, False):
    for a in ("report", "xcov"):
        trycov(verbose=v, annotate=a)

thistest.result()
