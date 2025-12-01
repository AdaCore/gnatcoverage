"""
Check that, in source instrumentation mode, --excluded-source-files can be used
when loading SID files, and that it does not interfere with the loading of
checkpoints otherwise.
"""

from SCOV.minicheck import build_and_run, checked_xcov, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches
import os


tmp = Wdir("tmp_")

# Generate a project, instrument it and run it
p = gprfor(mains=["main.adb"], srcdirs=[".."])
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt",
    mains=["main"],
    trace_mode="src",
    extra_coverage_args=[],
)
xcov_args_no_trace = xcov_args[:-1]
trace_file = xcov_args[-1]

expected_res_dict = {
    "pkg.adb.xcov": {"-": {7}},
    "pkg-say_goodbye.adb.xcov": {"-": {9}},
    "pkg.ads.xcov": {},
}


def check(report_name, args, expected_files):
    """
    Run gnatcov coverage with the given set of arguments in xcov mode,
    and store the results in directory 'report_name'. Check that a report is
    generated exactly for the files listed in 'expected_files', given the
    fact that the results for a file should be the same accross all runs
    and than coverage results match corresponding ones in expected_res_dict.
    """
    thistest.log(f"== {report_name} ==")

    # Create report dir to avoid warning message
    os.mkdir(report_name)
    checked_xcov(
        args + ["-axcov", "--output-dir=" + report_name],
        f"{report_name}_out.txt",
    )
    expected = {
        filename: expected_res_dict[filename] for filename in expected_files
    }
    expected["main.adb.xcov"] = {"+": {5, 6}}
    check_xcov_reports(report_name, expected, discard_empty=False)


# Check that not passing the option has no effect
check(
    "report0",
    xcov_args + ["--save-checkpoint=full.ckpt"],
    ["pkg.adb.xcov", "pkg-say_goodbye.adb.xcov", "pkg.ads.xcov"],
)

# Check that --excluded-source-files has the expected effect on the 'coverage'
# command.
check(
    "report1",
    xcov_args + ["--excluded-source-files=pkg-say_goodbye.adb"],
    ["pkg.adb.xcov", "pkg.ads.xcov"],
)

# Check that --excluded-source-files does not filter checkpoint loading
check(
    "report2",
    xcov_args_no_trace
    + ["--excluded-source-files=pkg-say_goodbye.adb", "-Cfull.ckpt"],
    ["pkg.adb.xcov", "pkg-say_goodbye.adb.xcov", "pkg.ads.xcov"],
)

# Check that in presence of bot a checkpoint and a SID file,
# --excluded-source-files filters the SID file but not the checkpoint.
checked_xcov(
    xcov_args
    + [
        "--excluded-source-files=pkg-say_goodbye.adb",
        "--save-checkpoint=pkg.ckpt",
    ],
    "report3_out.txt",
)
check(
    "report3",
    xcov_args + ["--excluded-source-files=pkg*.adb", "-Cpkg.ckpt"],
    ["pkg.adb.xcov", "pkg.ads.xcov"],
)

# Check that if a file is ignored when creating the checkpoint, then it is not
# present when loading that checkpoint.
checked_xcov(
    xcov_args
    + [
        "--save-checkpoint=pkg_goodbye.ckpt",
        "--excluded-source-files=pkg.adb",
    ],
    "report4_out.txt",
)
check(
    "report4",
    xcov_args_no_trace + ["-Cpkg_goodbye.ckpt"],
    ["pkg-say_goodbye.adb.xcov", "pkg.ads.xcov"],
)

thistest.result()
