"""
Check the selection of projects of interest, with combinations of
-P, --projects, --no-subprojects and externally built.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.control import env, gnatemu_board_name
from SUITE.cutils import Wdir, lines_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import xcov

import os
import e3.fs


root_project = os.path.abspath("root.gpr")
board = gnatemu_board_name(env.target.machine)
board_arg = "-XBOARD={}".format(board)
wd = Wdir("tmp_")

# Every project of interest is setup to feature a single unit named after the
# project. Dumping the units of interest for a given analysis, with
# --dump-units-to then indirectly gives us the list of projects of interest.

# --dump-units-to is a gnatcov coverage option. We just need to build and run
# the dummy "root" program once, and we can then proceed with all the coverage
# --dump-units-to tests we need.

cov_cmdline = build_and_run(
    gprsw=GPRswitches(
        root_project=root_project,
        xvars=[("BOARD", board)],
    ),
    mains=["root"],
    covlevel="stmt",
    gpr_obj_dir="../obj",
    gpr_exe_dir="../obj",
    extra_args=[board_arg],
    extra_coverage_args=[],
)

trace = cov_cmdline[-1]


def check(options, xunits):
    """
    Check that running gnatcov coverage with the provided list of
    `options` for project selection yields `xunits` as the set of
    units of interest, conveyed as a list. Verify the list of units
    reported by --dump-units-to and the set of produced .xcov reports.
    """

    # Compute the name of a unique output dir for the reports,
    # based on the options.
    opt_str = "".join(
        [
            opt.replace("--projects=", "")
            .replace("root", "r")
            .replace("../", "")
            .replace(".gpr", "")
            .replace("_X=True", "")
            .replace("--no-subprojects", "ns")
            .strip("-")
            for opt in options
        ]
    )

    odir = "tmp_" + opt_str
    ofile = os.path.join(odir, "dump-units")

    xcov(
        [
            "coverage",
            trace,
            "--level=stmt",
            "--annotate=xcov",
            "--output-dir={}".format(odir),
            "--dump-units-to={}".format(ofile),
            board_arg,
        ]
        + options,
        out="cov-" + opt_str + ".log",
    )

    # Primary check, from the list of units reported by --dump-units-to

    xunits = set(xunits)
    runits = set(lines_of(ofile))

    thistest.fail_if(
        runits != xunits,
        "for options '{}', reported list of units not as expected:\n"
        "expected: {}\n"
        "obtained: {}".format(" ".join(options), xunits, runits),
    )

    # Secondary check, from the set of generated reports

    # We have a list/set of expected unit names on the one hand, and a set of
    # .xcov files in `odir` on the other hand. In the latter set, we can have
    # multiple files for a unit, for example a spec and a body for a package.
    # Map the report files to a set of units by stripping both the leading
    # subdir part and the extension.

    runits = {
        os.path.basename(r).split(".")[0]
        for r in e3.fs.ls(os.path.join(odir, "*.xcov"))
    }

    thistest.fail_if(
        runits != xunits,
        "for options '{}', list of units from reports not as expected:\n"
        "expected: {}\n"
        "obtained: {}".format(" ".join(options), xunits, runits),
    )


# -P alone, recursive or not
# --------------------------

# No externally built

check(
    options=["-P", "../root.gpr"],
    xunits=["root", "ssa", "a1", "ssb", "b1", "common"],
)

check(options=["-P", "../root.gpr", "--no-subprojects"], xunits=["root"])

check(options=["-P", "../ssa.gpr"], xunits=["ssa", "a1", "common"])

check(options=["-P", "../ssa.gpr", "--no-subprojects"], xunits=["ssa"])

# ssa externally built

check(
    options=["-P", "../root.gpr", "-XSSA_X=True"],
    xunits=["root", "a1", "ssb", "b1", "common"],
)


# -P --projects, recursive or not
# -------------------------------

# No externally built

# Recursive from single child of root, not including root
check(
    options=["-P", "../root.gpr", "--projects=ssa"],
    xunits=["ssa", "a1", "common"],
)

check(
    options=["-P", "../root.gpr", "--projects=ssb"],
    xunits=["ssb", "b1", "common"],
)

# Non recursive from single child of root, not including root
check(
    options=["-P", "../root.gpr", "--projects=ssa", "--no-subprojects"],
    xunits=["ssa"],
)

check(
    options=["-P", "../root.gpr", "--projects=ssb", "--no-subprojects"],
    xunits=["ssb"],
)

# Non recursive from multiple children of root, not including root
check(
    options=[
        "-P",
        "../root.gpr",
        "--projects=ssa",
        "--projects=ssb",
        "--no-subprojects",
    ],
    xunits=["ssa", "ssb"],
)

check(
    options=[
        "-P",
        "../root.gpr",
        "--projects=ssa",
        "--projects=b1",
        "--no-subprojects",
    ],
    xunits=["ssa", "b1"],
)

# Non recursive including root
check(
    options=[
        "-P",
        "../root.gpr",
        "--projects=root",
        "--projects=ssb",
        "--no-subprojects",
    ],
    xunits=["root", "ssb"],
)

# ssb externally built

check(
    options=["-P", "../root.gpr", "--projects=ssb", "-XSSB_X=True"],
    xunits=["b1", "common"],
)

thistest.result()
