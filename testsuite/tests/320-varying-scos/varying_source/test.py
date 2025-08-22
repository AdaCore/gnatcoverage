"""
Check that gnatcov rejects the checkpoint information for different source
versions.
"""

from e3.fs import mkdir

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

root_project = gprfor(srcdirs=["."], langs=["ada"], mains=["main.adb"])


def generate_checkpoint(source_body, ckpt_name):

    with open("main.adb", "w+") as f:
        f.write(
            f"""procedure Main is
            begin
            {source_body}
            end Main;"""
        )

    build_run_and_coverage(
        gprsw=GPRswitches(root_project=root_project),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=[
            "--cancel-annotate",
            "--save-checkpoint",
            ckpt_name,
        ],
        trace_mode="src",
    )


generate_checkpoint("null;", "v1.ckpt")
generate_checkpoint("null;\nnull;", "v2.ckpt")

coverage_log = "coverage.log"
mkdir("xcov")
p = xcov(
    [
        "coverage",
        "-P",
        root_project,
        "--level=stmt",
        "-Cv1.ckpt",
        "-Cv2.ckpt",
        "-axcov",
        "--output-dir",
        "xcov",
    ],
    out=coverage_log,
    register_failure=False,
)

thistest.fail_if_no_match(
    "Unexpected error message from 'gnatcov coverage'.",
    regexp=r"warning: unexpected fingerprint, cannot merge coverage"
    r" information for main.adb \(from v2.ckpt\)",
    actual=contents_of(coverage_log),
)

check_xcov_reports("xcov", {"main.adb.xcov": {"+": {3}}})
thistest.result()
