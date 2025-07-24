"""
Check that gnatcov works fine on projects using non-standard naming schemes.
"""

from e3.fs import cp

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


spec_suffix = ".1.ada"
body_suffix = ".2.ada"

for casing, converter in [
    ("lowercase", str.lower),
    ("uppercase", str.upper),
    ("mixedcase", str),
]:
    tmp = Wdir(f"tmp_{casing}")

    pkg_spec = converter("Pkg") + spec_suffix
    pkg_body = converter("Pkg") + body_suffix
    child_spec = converter("Pkg__Child") + spec_suffix
    child_body = converter("Pkg__Child") + body_suffix

    cp("../main.adb", "main.adb")
    cp("../pkg.1.ada", pkg_spec)
    cp("../pkg.2.ada", pkg_body)
    cp("../pkg__child.1.ada", child_spec)
    cp("../pkg__child.2.ada", child_body)

    p = gprfor(
        mains=["main.adb"],
        prjid="p",
        srcdirs=["."],
        extra=f"""
            package Naming is
                for Spec_Suffix ("Ada") use "{spec_suffix}";
                for Body_Suffix ("Ada") use "{body_suffix}";
                for Dot_Replacement use "__";
                for Body ("main") use "main.adb";
                for Casing use "{casing}";
            end Naming;
        """,
    )
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=p),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["-axcov", "--output-dir=report"],
    )

    check_xcov_reports(
        "report",
        {
            "main.adb.xcov": {"+": {5}},
            f"{pkg_spec}.xcov": {},
            f"{pkg_body}.xcov": {"+": {4}},
            f"{child_spec}.xcov": {"+": {3}},
            f"{child_body}.xcov": {"+": {6}},
        },
        discard_empty=False,
    )

    tmp.to_homedir()

thistest.result()
