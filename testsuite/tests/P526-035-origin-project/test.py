"""
Check that gnatcov correctly handles the Origin_Project project attribute.
"""

import glob
import os
import os.path

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, tracename_for

tmp = Wdir("tmp_")


def xcov_list():
    """
    Return the list of *.xcov files in the "obj" subdirectory.
    """
    return set(
        os.path.basename(f) for f in glob.glob(os.path.join("obj", "*.xcov"))
    )


def clean_xcov():
    """
    Remove all *.xcov files in the "obj" subdirectory.
    """
    for filename in glob.glob(os.path.join("obj", "*.xcov")):
        os.remove(filename)


gprfor(prjid="p1", srcdirs=["../src-p1"], mains=None, langs="Ada")

p2 = gprfor(
    prjid="p2",
    srcdirs=["../src-p2"],
    mains=["p2.adb"],
    deps=["p1"],
    extra='for Origin_Project use "p1";',
)

# Check that the Origin_Project provides a default for coverage analysis and
# that this default is overiden by the --projects switch.
for args, xcovs in [
    ([], {"p1.adb.xcov"}),
    (["--projects=p2"], {"p2.adb.xcov"}),
    (["--projects=p1", "--projects=p2"], {"p1.adb.xcov", "p2.adb.xcov"}),
]:
    clean_xcov()

    # Build the projects and produce a trace for the main
    # Passing the same --projects option as given to `gnatcov coverage`
    # to be consistent with gnatcov use model.
    # If these options are not accounted at instrumentation time,
    # there won't be SID files for every wanted unit of interest.
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=p2),
        covlevel="stmt",
        mains=["p2"],
        extra_coverage_args=["-a", "xcov", "--no-subprojects"],
        extra_args=args,
    )

    p2_trace = tracename_for("p2")

    expected = (
        "\n".join("* {}".format(f) for f in sorted(xcovs)) or "  <empty>"
    )
    got = (
        "\n".join("* {}".format(f) for f in sorted(xcov_list())) or "  <empty>"
    )

    thistest.fail_if(
        expected != got,
        "[{}switches: {}]\n"
        "Unexpected XCOV files. Expected:\n"
        "{}\n"
        "But got instead:\n"
        "{}\n".format(p2_trace, " ".join(args), expected, got),
    )

thistest.result()
