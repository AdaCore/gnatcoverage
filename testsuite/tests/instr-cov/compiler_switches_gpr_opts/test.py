"""
Check that compiler switches are properly imported from project files.
GNATcoverage used to ignore Compiler.Switches when it was used to specify
language specific compiler switches.
"""

from SCOV.minicheck import xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

# Some automatic dump triggers (ravenscar-task-termination) are not available
# for all languages: instrumenting a C/C++ project with the default dump
# trigger in some configurations yields a warning. Explicitly ask for a dump
# trigger that is always supported to avoid this warning.
xcov_args = ["instrument", "--level=stmt", "--dump-trigger=main-end"]


def check_lang(lang, file_ext):
    def gprfor_wrapper(prjid, compiler_extra):
        return gprfor(
            prjid=prjid,
            srcdirs=[f"../src-{lang}"],
            mains=[f"main.{file_ext}"],
            compiler_extra=compiler_extra,
        )

    # Check using the Compiler.Default_Switches attribute for language specific
    # compiler switches.
    prj_lds = gprfor_wrapper(
        prjid=f"lang_default_switches_{file_ext}",
        compiler_extra=f"""
        for Default_Switches ("{lang}") use ("-DA", "-DB");
        """,
    )
    xcov(xcov_args + ["-P", prj_lds])

    # Check using the Compiler.Switches attribute for language specific
    # compiler switches.
    prj_ls = gprfor_wrapper(
        prjid=f"lang_switches_{file_ext}",
        compiler_extra=f"""
        for Switches ("{lang}") use ("-DA", "-DB");
        """,
    )
    xcov(xcov_args + ["-P", prj_ls])

    # Check using the Compiler.Switches attribute both for file specific
    # compiler switches.
    prj_fs = gprfor_wrapper(
        prjid=f"file_switches_{file_ext}",
        compiler_extra=f"""
        for Default_Switches ("{lang}") use ("-DC");
        for Switches ("{lang}") use ("-DC");
        for Switches ("main.{file_ext}") use ("-DA", "-DB");
        """,
    )
    xcov(xcov_args + ["-P", prj_fs])


check_lang("c", "c")
check_lang("c++", "cpp")

thistest.result()
