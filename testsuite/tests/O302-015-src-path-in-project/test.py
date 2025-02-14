"""
Check that we can build, run and analyse a program featuring dummy units with
or without a project file, relying on debug info to locate the sources in the
case.
"""

import os.path

from e3.fs import rm

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


bin_traces = thistest.options.trace_mode == "bin"


# Precompute absolute paths before going to the temporary directory
prj_path = os.path.abspath("p.gpr")
obj_dir = os.path.abspath("obj")
pkg_ads_report_path = os.path.join(obj_dir, "pkg.ads.xcov")
scos = [
    os.path.join(obj_dir, name) for name in ("pkg", "pkg-hw", "pkg-io", "main")
]

wd = Wdir(subdir="tmp_")


# Build the project and produce a trace for its main. This is using MCDC + SCOs
# as in the original test we received (when SCOs are available, i.e. in binary
# trace mode).
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=prj_path),
    covlevel="stmt+mcdc",
    mains=["main"],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    scos=scos,
    extra_coverage_args=["--annotate=xcov"],
)
trace_file = xcov_args[-1]


# Now check that analysis works one way or the other: with SCOs and with -P in
# binary trace mode, and with -P only in other modes.


def try_coverage(xcov_args, use_project):
    thistest.log(f"== try_coverage(use_project={use_project})")
    rm(pkg_ads_report_path)

    if not use_project:
        xcov_args = xcov_args + ["--output-dir={}".format(obj_dir)]
    p = xcov(xcov_args)

    thistest.fail_if(
        p.out, '"gnatcov coverage" output is not empty:\n--\n{}'.format(p.out)
    )
    thistest.fail_if(
        not os.path.isfile(pkg_ads_report_path),
        '"gnatcov coverage" could not produce a report file for pkg.ads',
    )


try_coverage(xcov_args, use_project=False)
if bin_traces:
    try_coverage(
        [
            "coverage",
            "-P",
            prj_path,
            "--level=stmt+mcdc",
            "--annotate=xcov",
            trace_file,
        ],
        use_project=True,
    )

thistest.result()
