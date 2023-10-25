"""
Check various error cases related to the use of preprocessing in Ada.
"""

import os
import os.path

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Avoid "creating output path" info messages
os.mkdir("obj")

for basename, expected_msg in [
    (
        "no_such_file",
        ".*gnatcov.*: error while loading preprocessor data from project"
        "\n.*gnatcov.*: no such file: .*no_such_file\\.txt",
    ),
    (
        "bad_syntax",
        ".*gnatcov.*: error while loading preprocessor data from project"
        "\n.*gnatcov.*: .*bad_syntax\\.txt:1:1: Ada source filename expected",
    ),
    (
        "eval_error",
        ".*gnatcov.*: instrumentation failed for .*pkg\\.ads"
        "\n.*gnatcov.*: please make sure the original project can be"
        " compiled"
        '\n.*gnatcov.*: pkg\\.ads:2:6: unknown symbol "Log"',
    ),
]:
    thistest.log(f"== {basename} ==")
    log_filename = f"{basename}-out.txt"
    p = xcov_instrument(
        gprsw=GPRswitches(
            root_project=gprfor(
                prjid=basename,
                mains=["main.adb"],
                srcdirs=[".."],
                compiler_extra=(
                    'for Default_Switches ("Ada")'
                    ' use ("-gnatep="'
                    " & Project'Project_Dir"
                    f' & "/../{basename}.txt");'
                ),
            )
        ),
        covlevel="stmt",
        register_failure=False,
        out=log_filename,
    )
    thistest.fail_if(p.status == 0, "'gnatcov instrument' is supposed to fail")
    output = contents_of(log_filename)
    thistest.fail_if_no_match(
        "'gnatcov instrument' output",
        expected_msg,
        contents_of(log_filename).strip(),
    )

thistest.result()
