"""
Test simple external annotations for buffer management, both for
buffer dump and buffer reset.
"""

from copy import deepcopy

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir()


def run_one(tc_id, annotation_file, expected_results):
    """
    Run a whole gnatcov source trace workflow, passing the
    given annotation_file to the --external-annotation switch
    and checking the expected_results.

    tc_id is used to identify the test case (temporary work dir)
    """
    thistest.log(f"====== Running test {tc_id} ======")
    tmp.to_subdir(f"tmp_{tc_id}")
    prjid = f"prj_{tc_id}"
    prj = gprfor(
        mains=["main.adb"],
        srcdirs=[".."],
        langs=["Ada", "C", "C++"],
        prjid=prjid,
    )
    build_run_and_coverage(
        gprsw=GPRswitches(prj),
        extra_instr_args=[
            f"--external-annotations={annotation_file}",
        ],
        extra_args=[
            "--units=Pkg",
            "--units=c_unit.c",
            "--units=cpp_unit.cpp",
        ],
        covlevel="stmt",
        mains=["main"],
        dump_trigger="manual",
        manual_prj_name=prjid,
        extra_coverage_args=["-axcov"],
    )
    check_xcov_reports(
        "obj",
        expected_results,
    )


default_cov = {
    "cpp_unit.cpp.xcov": {"-": {6}},
    "c_unit.c.xcov": {"-": {4}},
    "pkg.ads.xcov": {},
    "pkg.adb.xcov": {"-": {5}},
}

# CPP first, we expect full coverage for the CPP unit as buffers are cleared
# when entering do_cpp_dump, and dumped at the end of that function
expected_cov = deepcopy(default_cov)
expected_cov["cpp_unit.cpp.xcov"] = {"+": {6}}
run_one("cpp", "../cpp.toml", expected_cov)

# C then, we expect full coverage for the C unit as buffers are cleared
# when entering do_c_dump, and dumped at the end of that function
expected_cov = deepcopy(default_cov)
expected_cov["c_unit.c.xcov"] = {"+": {4}}
run_one("c", "../c.toml", expected_cov)

# For Ada, first check the annotations when the file is part of the set of
# units of interest. As above, buffers are cleared entering the Do_Dump
# procedure, then dumped at the end.
expected_cov = deepcopy(default_cov)
expected_cov["pkg.adb.xcov"] = {"+": {5}}
run_one("ada_unit", "../ada_unit.toml", expected_cov)

# Test when the annotations are not part of a unit of interest. As the buffers
# are handled in the same fashion, no code in a unit of interest should be
# executed.
run_one("ada_no_unit", "../ada_no_unit.toml", default_cov)

thistest.result()
