"""
Basic test on the --source-search option
"""

import os

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, empty
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


Wdir("tmp_")

xcov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(["test_engines.adb"], srcdirs="../src")
    ),
    covlevel="stmt",
    mains=["test_engines"],
    extra_coverage_args=["--annotate=xcov"],
    scos=["obj/engines"],
)

xcov(xcov_args + ["--source-search=./src"], out="warnings1.txt")
thistest.fail_if(not empty("warnings1.txt"), "no source warning")

os.rename("../src", "../newsrc")

xcov(xcov_args + ["--source-search=../newsrc"], out="warnings2.txt")
thistest.fail_if(not empty("warnings2.txt"), "Source search unsuccessful")

# Same test but with a response file
source_search_filename = os.path.abspath("src-search.txt")
with open(source_search_filename, "w") as f:
    f.write(os.path.join("..", "newsrc"))

xcov(
    xcov_args + [f"--source-search=@{source_search_filename}"],
    out="warnings3.txt",
)
thistest.fail_if(
    not empty("warnings3.txt"), "Source search unsuccessful (response file)"
)

os.rename("../newsrc", "../src")

thistest.result()
