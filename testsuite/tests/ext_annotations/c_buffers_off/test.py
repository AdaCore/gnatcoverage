"""
Check that external annotation linenos that cannot be mapped to lines in a C
source file are correctly warned about/ignored. They used to trigger a crash.
"""

import os.path
import re

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

main_c = os.path.abspath("../main.c")
expected_msg = (
    f"warning: {main_c} has no 12:1 location: ignoring the RESET_BUFFERS"
    " external annotation"
)

# gnatcov expects absolute paths in line directives, so it will not realize
# that "main.c" (just the basename) is source code "of interest". As a result,
# there is no coverage obligation for the "main" function.
build_run_and_coverage(
    gprsw=GPRswitches(gprfor(mains=["main.c"], srcdirs=[".."], langs=["C"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
    extra_instr_args=["--external-annotations=../c.toml"],
    dump_trigger="manual",
    tolerate_instrument_messages=re.escape(expected_msg),
    manual_prj_name="gen",
)
check_xcov_reports("obj", {"main.c.xcov": {"+": {4}}})

thistest.fail_if_not_equal(
    '"gnatcov instrument" output',
    contents_of("instrument.log").strip(),
    expected_msg,
)

thistest.result()
