"""
Check that the right column numbers are output for a statement coverage
obligation in the XML report.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains="main.adb")),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-a", "xml"],
)

if thistest.options.trace_mode == "src":
    expected_match = (
        '<line num="5" column_begin="4" column_end="22" '
        'src="   Put_Line (&quot;Hello&quot;);"/>'
    )
else:
    expected_match = (
        '<line num="5" column_begin="4" column_end="19" '
        'src="   Put_Line (&quot;Hello"/>'
    )

thistest.fail_if(expected_match not in contents_of("obj/main.adb.xml"))
thistest.result()
