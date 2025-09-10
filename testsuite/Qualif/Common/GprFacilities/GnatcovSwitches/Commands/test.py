import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, empty
from SUITE.tutils import exepath_to, gprbuild, gprfor, xcov, xrun
from SUITE.gprutils import Csw, gprcov_for


# Check correctness of transmission from Switches to commands for a few
# particular commands. Check that Switches ("*") comes before Switches (cmd).

wd = Wdir("tmp_")


# We will be exercising combinations of run/coverage operations with option
# variations controlled via Coverage attributes in an otherwise common project
# file for a simple program.


def gprvariant(prjid, extra):
    return gprfor(
        prjid=prjid, srcdirs=["../src"], mains=["p.adb"], extra=extra
    )


exe = exepath_to("p")

# Build once
gprbuild(gprvariant(prjid="bld", extra=""))

# ------------------------------------------------
# -- Simple helpers for coverage/run variations --
# ------------------------------------------------
#
# --tag, valid only for gnatcov run.
# --annotate, valid only for gnatcov coverage.


def tag_for(prjid):
    return "tag-%s" % prjid


def tagopt_for(prjid):
    return ["--tag=%s" % tag_for(prjid)]


def rep_for(prjid):
    return "%s.rep" % prjid


def annopt_for(prjid):
    return ["--annotate=report"]


# level expected to be assessed eventually, always
lev = "stmt+decision"


def levopt_for(prjid):
    return ["--level=%s" % lev]


def try_run(prjid, gpr):
    """gnatcov run with common set of options & variations via gpr."""
    log = prjid + ".rlog"
    xrun(
        ["-P%s" % gpr, exe, "-o", "%s.trace" % prjid],
        out=log,
        register_failure=False,
    )
    return contents_of(log)


def try_cov(prjid, gpr):
    """
    gnatcov coverage with common set of options & variations via gpr.  Expect
    valid options and check for commonly expected outcome
    """
    log = prjid + ".clog"
    xcov(
        ["coverage", "-P%s" % gpr, "%s.trace" % prjid, "-o", rep_for(prjid)],
        out=log,
        register_failure=False,
    )

    # Check that we get a report with expected contents wrt options
    # eventually. The tag & level checks make sure that
    #
    # * options intended for run do get there and not to coverage
    # * options intended for coverage do get there and not to run
    thistest.fail_if(not empty(log), "unexpected contents in %s" % log)

    rep = contents_of(rep_for(prjid))
    thistest.fail_if(
        "level: %s" % lev not in rep,
        "missing expected level indication in %s" % rep_for(prjid),
    )

    thistest.fail_if(
        not re.search("tag.*: %s" % tag_for(prjid), rep),
        "missing expected tag indication in %s" % rep_for(prjid),
    )


def check_valid_sequence_for(prjid, gprcov):
    """
    Common checking sequence for a specific gpr coverage package
    with valid options for both run and coverage.
    """
    gpr = gprvariant(prjid=prjid, extra=gprcov)
    try_run(prjid, gpr)
    try_cov(prjid, gpr)


# Basic check for starters. No "*".
prjid = "basic"
check_valid_sequence_for(
    prjid=prjid,
    gprcov=gprcov_for(
        switches=[
            Csw("run", tagopt_for(prjid)),
            Csw("coverage", levopt_for(prjid) + annopt_for(prjid)),
        ]
    ),
)

# Check that "*" applies to all. Pass --level there.
prjid = "star_valid"
check_valid_sequence_for(
    prjid=prjid,
    gprcov=gprcov_for(
        switches=[
            Csw("*", levopt_for(prjid)),
            Csw("run", tagopt_for(prjid)),
            Csw("coverage", annopt_for(prjid)),
        ]
    ),
)

# Check that command specific args prevail over "*", with "*" placed
# before in the gpr file.
prjid = "star_postover"
check_valid_sequence_for(
    prjid=prjid,
    gprcov=gprcov_for(
        switches=[
            Csw("*", ["--level=stmt+mcdc"]),
            Csw("run", tagopt_for(prjid)),
            Csw("coverage", levopt_for(prjid) + annopt_for(prjid)),
        ]
    ),
)

# Likewise, with "*" placed after in the gpr file.
prjid = "star_preover"
check_valid_sequence_for(
    prjid=prjid,
    gprcov=gprcov_for(
        switches=[
            Csw("run", tagopt_for(prjid)),
            Csw("coverage", levopt_for(prjid) + annopt_for(prjid)),
            Csw("*", ["--level=stmt+mcdc"]),
        ]
    ),
)


# Check that "*" applies to all. Pass invalid for run, check failure.
prjid = "star_invalid"
gpr = gprvariant(
    prjid=prjid, extra=gprcov_for(switches=[Csw("*", annopt_for(prjid))])
)
rlog = try_run(prjid, gpr)

thistest.fail_if(
    '--annotate is not valid with the "run" command.' not in rlog,
    "missing expected failure indication in run log for %s" % prjid,
)

thistest.result()
