"""
Regression test: check that we do not preprocess multiple times a file at
coverage time.
"""

import re

from SCOV.minicheck import build_run_and_coverage
from SUITE.control import env
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=[".."], mains=["test_macros.c"])
    ),
    covlevel="stmt+mcdc",
    mains=["test_macros"],
    extra_coverage_args=["--annotate=xcov+", "-v"],
)

thistest.fail_if(
    len(
        re.findall(
            f"gcc{env.build.os.exeext} -E", contents_of("coverage.log")
        )
    )
    != 1,
    "source file was preprocessed multiple times",
)

thistest.result()
