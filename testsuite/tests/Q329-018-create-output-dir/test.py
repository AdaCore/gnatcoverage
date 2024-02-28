import os.path

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov


src_dir = os.path.abspath(".")
wd = Wdir("tmp_")
output_dir = os.path.join("report", "foo")

# Generate a project for our sources, build it and generate a trace for it
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(["foo.adb"], srcdirs=src_dir)),
    covlevel="stmt",
    mains=["foo"],
    extra_coverage_args=["-axcov", "--output-dir={}".format(output_dir)],
)

# Now, try to produce a report in a directory that does not exist (even its
# parent does not exist!).
assert not os.path.exists("report")
xcov(xcov_args, "coverage.log")

# Check that gnatcov reports that it creates these directories
thistest.fail_if(
    "info: creating output path " not in contents_of("coverage.log"),
    "gnatcov does not say it is creating directories, it should",
)

# Finally, check we have the expected reports
check_xcov_reports(output_dir, {"foo.adb.xcov": {"+": {5}}})
thistest.result()
