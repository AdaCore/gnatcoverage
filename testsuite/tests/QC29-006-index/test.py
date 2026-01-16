"""
Test that the coverage indices generated along the xcov and report format
reports contains the correct information.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov, srctracename_for
from SUITE.gprutils import GPRswitches

import glob


def create_lib_gpr(name, srcdir, objdir, deps=None, extra=""):
    """
    Create a gpr file for the library.

    :param name: Name of the library.
    """

    return gprfor(
        mains=[],
        prjid=name,
        srcdirs=[srcdir],
        objdir=objdir,
        langs=["Ada"],
        deps=deps,
        extra=extra,
    )


tmp = Wdir("tmp_")

# Create the GPR file for the root project
src_gpr = gprfor(
    mains=["main.adb"],
    prjid="main",
    srcdirs=["../src-root"],
    objdir="obj",
    langs=["Ada"],
    deps=["foo", "lib"],
)

# Create the GPR files for the libraries
lib_gpr = create_lib_gpr(
    "lib",
    "../src-lib",
    "obj-lib",
    extra="""
                               for Library_Name use "lib";
                               for Library_Dir use "lib";
                               """,
)
foo_gpr = create_lib_gpr(
    "foo",
    "../src-root/src-foo",
    "obj-foo",
    ["bar", "lib"],
    extra="""
        for Library_Name use Project'Name;
        for Library_Dir use "lib-" & Project'Name;
    """,
)
bar_gpr = create_lib_gpr("bar", "../src-root/src-foo/src-bar", "obj-bar")

gprsw = GPRswitches(root_project=src_gpr)


def check_indices(metrics, statsdir, refdir=""):
    """
    Check that the index file corresponding to each coverage metric in
    "metrics" contains the expected statistics.
    """

    for metric in metrics:
        filename = f"{metric}.index"
        idx = f"{statsdir}/{filename}"
        ref = f"{refdir}/{filename}"
        ref_path = f"../{ref}"

        if thistest.options.trace_mode == "src":
            thistest.fail_if_diff(
                ref_path,
                idx,
                f"unexpected index content for {filename} "
                f"with baseline {ref}",
            )
        else:
            idx_contents = contents_of(idx)
            ref_contents = (
                r"(.|\n)*bar:"
                r"\n        - 100%, !   0%, \+   0%     out of 15 lines"
                r"\n"
                r"\n    bar.adb:"
                r"\n        - 100%, !   0%, \+   0%     out of 15 lines"
                r"\n"
                r"\nfoo:"
                r"\n        - 100%, !   0%, \+   0%     out of 8 lines"
                r"\n"
                r"\n    foo.adb:"
                r"\n        - 100%, !   0%, \+   0%     out of 8 lines"
                r"\n"
                r"\nlib:"
                r"\n        -   0%, !  66%, \+  33%     out of 3 lines"
                r"\n"
                r"\n    lib.ads:"
                r"\n        -   0%, !  66%, \+  33%     out of 3 lines"
                r"\n"
                r"\nmain:"
                r"\n        -   0%, !  10%, \+  90%     out of 10 lines"
                r"\n"
                r"\n    a.adb:"
                r"\n        -   0%, !   0%, \+ 100%     out of 3 lines"
                r"\n    a.ads:"
                r"\n        -   -%, !   -%, \+   -%     out of 0 lines"
                r"\n    main.adb:"
                r"\n        -   0%, !  14%, \+  85%     out of 7 lines"
                r"(.|\n)*"
            )
            thistest.fail_if_no_match(
                f"Unexpected coverage index {idx}" " content",
                ref_contents,
                idx_contents,
            )


if thistest.options.trace_mode == "src":
    # Instrument and run coverage analysis, then check that the coverage stats
    # are correct.
    build_run_and_coverage(
        gprsw=gprsw,
        covlevel="stmt+mcdc+atcc",
        mains=["main"],
        extra_coverage_args=["--annotate=xcov,report"],
        out="coverage.log",
    )

    metrics = ["lines", "stmt", "decision", "mcdc", "atc", "atcc"]
    check_indices(metrics, "obj/stats", "ref")

    # Now compute the coverage by using .sid files. There is no project
    # information available for the sources and we expected their stats to be
    # displayed under the "Other sources" category.
    objdirs = glob.glob("obj*")
    sids = []
    for objdir in objdirs:
        sids += glob.glob(f"{objdir}/*.sid")

    xcov(
        [
            "coverage",
            "--level=stmt+mcdc+atcc",
            "--annotate=xcov,report,cobertura",
        ]
        + ["--sid=" + file for file in sids]
        + ["--trace=" + srctracename_for("main")],
        out="coverage.log",
    )

    check_indices(metrics, "stats", "ref-sid")

else:
    metrics = ["lines"]

    build_run_and_coverage(
        gprsw=gprsw,
        covlevel="branch",
        mains=["main"],
        extra_coverage_args=["--annotate=xcov+,cobertura"],
        out="couverage.log",
    )

    check_indices(metrics, "obj/stats")

thistest.result()
