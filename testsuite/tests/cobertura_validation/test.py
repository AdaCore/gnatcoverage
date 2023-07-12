"""
Check that creating Cobertura coverage reports works as expected.
"""

import os

from e3.fs import sync_tree, mv

from lxml import etree

from SCOV.minicheck import build_and_run, xcov
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest


tmp = Wdir("tmp_")

sync_tree(os.path.join("..", "src"), "src")

gprsw = gprsw = GPRswitches(root_project=gprfor(srcdirs=["src"], mains=["test.adb"]))

scos_list = ["obj/lib.c", "obj/pkg", "obj/test"]


def build_run_coverage_and_check(
    covlevel, output_opt=None, source_root=None, hide_src=False, expected_filenames=None
):
    # We need to use manually designated SCOs if we are to remove the sources
    # from the project.
    #
    # Passing explicit SCOs does not work in bin traces for C files, so do not
    # run cases with explicit SCOs if this is a bin-traces run.
    if thistest.options.trace_mode == "bin" and hide_src:
        return
    extra_args = ["--annotate=cobertura"]
    if source_root:
        extra_args += ["--source-root", source_root]

    filename = output_opt if output_opt else "cobertura.xml"
    if output_opt:
        extra_args += ["-o", output_opt]
    # Using explicit SCOs instead of a project changes the default output dir,
    # so set it manually to keep everything consistent.
    if hide_src:
        extra_args += ["--output-dir=obj"]

    xcov_args = build_and_run(
        gprsw=gprsw,
        covlevel=covlevel,
        scos=scos_list if hide_src else None,
        mains=["test"],
        extra_coverage_args=extra_args,
    )
    if hide_src:
        mv("src", "src_hidden")
    xcov(xcov_args, out="coverage.log")
    if hide_src:
        mv("src_hidden", "src")
    parser = etree.XMLParser(dtd_validation=True)
    report = etree.parse(os.path.join("obj", filename), parser)
    if expected_filenames:
        filenames = set()
        for elt in report.getroot().iter():
            if elt.tag == "class":
                filenames.add(elt.get("filename"))

        thistest.fail_if_not_equal(
            "list of filenames in cobertura.xml",
            "\n".join(sorted(expected_filenames)),
            "\n".join(sorted(filenames)),
        )


# For both source and object coverage, check that the output "cobertura.xml"
# file is valid according to the cobertura.dtd standard.
build_run_coverage_and_check("stmt+mcdc")
if thistest.options.trace_mode == "bin":
    build_run_coverage_and_check("insn", "cobertura-insn.xml")
    build_run_coverage_and_check("branch", "cobertura-branch.xml")


# Check that the --source-root option works as expected
expected_filenames = {"lib.c", "pkg.adb", "pkg.ads", "test.adb"}
for prefix in ["src", "src/"]:
    thistest.log(f"== Prefix: {prefix} ==")

    build_run_coverage_and_check(
        "stmt", source_root=os.path.join(os.getcwd(), prefix), expected_filenames=expected_filenames
    )

# Check that the report can be emitted in the absence of the project sources. We
# have to rely on sid files to convey the coverage obligations
thistest.log("== Report with no sources ==")
# No pkg.ads in the report as there are no coverage obligations for it
expected_filenames = {"lib.c", "pkg.adb", "test.adb"}

build_run_coverage_and_check(
    "stmt",
    "cobertura-no_src.xml",
    source_root=os.path.join(os.getcwd(), "src"),
    hide_src=True,
    expected_filenames=expected_filenames
)

thistest.result()
