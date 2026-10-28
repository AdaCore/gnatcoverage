"""
Check that the --dump-filename-env-var, --dump-filename-prefix and
--dump-filename-simple options work as expected.
"""

import os

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches

from test_support import testcases


for tc in testcases:
    thistest.log(f"==== {tc.test_name} ====")

    wd = Wdir(f"tmp_{tc.test_name}")
    os.mkdir("traces-dir")
    try:
        env = dict(os.environ)
        env.update(tc.extra_env_vars)

        xcov_args = build_and_run(
            gprsw=GPRswitches(
                root_project=gprfor(srcdirs=[".."], mains=["main.adb"])
            ),
            covlevel="stmt",
            mains=["main"],
            extra_instr_args=tc.extra_args,
            extra_coverage_args=["-axcov", "--output-dir=xcov"],
            trace_mode="src",
            program_env=env,
            # The very goal of this testcase is to produce trace files with
            # non-default names. build_and_run cannot deal with them, so skip
            # checks here: the computing below are enough to check that the
            # expected source trace file was produced.
            register_failure=False,
        )

        traces = tc.find_traces()
        xcov(xcov_args + traces, out="coverage.log")
        check_xcov_reports("xcov", {"main.adb.xcov": {"+": {5}}})
    finally:
        wd.to_homedir()

thistest.result()
