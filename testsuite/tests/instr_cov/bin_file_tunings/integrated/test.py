"""
Check that the --dump-filename-env-var, --dump-filename-prefix,
--dump-filename-tag and --dump-filename-simple options work as expected in the
context of integrated instrumentation.
"""

import os.path

from e3.fs import cp

from SCOV.minicheck import check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.integrated_instr_utils import (
    MakefileMain,
    find_files_with_exts,
    setup_integration,
)
from SUITE.tutils import run_cov_program, thistest, xcov
from test_support import testcases


wd = Wdir()

for tc in testcases:
    thistest.log(f"==== {tc.test_name} ====")
    wd.to_subdir(f"tmp_{tc.test_name}")

    os.mkdir("traces-dir")
    cp(os.path.join("..", "*.c"), ".")

    setup_dir = os.path.abspath("setup-prefix")

    # Instrument/build the test program wih the requested switches
    build_env = setup_integration(
        files_of_interest=["pkg.c"],
        covlevel="stmt",
        output_dir=setup_dir,
        extra_args=tc.extra_args,
    )
    wf = MakefileMain(build_target_deps=["main.o", "pkg.o"])
    wf.build(build_env)

    # Run the test program to produce the traces
    run_cov_program(wf.get_executable(), out="run.log", env=tc.extra_env_vars)

    # Then check both the list of trace filenames, and the coverage report we
    # get from them.
    traces = tc.find_traces()
    if not traces:
        continue
    sid_files = find_files_with_exts([".sid"], setup_dir)
    xcov_args = [
        "coverage",
        "--level=stmt",
        "--annotate=xcov",
        "--output-dir=xcov",
        *[f"--sid={filename}" for filename in sid_files],
    ]
    xcov(xcov_args + traces, out="coverage.log")
    check_xcov_reports("xcov", {"pkg.c.xcov": {"+": {4}}})

thistest.result()
