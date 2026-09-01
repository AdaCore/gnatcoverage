"""
Check that gnatcov instrument emits a warning when excluding all of the unit
parts by means of --excluded-source-files.

Also check that gnatcov coverage emits a "missing SID file" warning.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

Wdir("tmp_")

p = gprfor(
    mains=["main.cpp"],
    srcdirs=[".."],
    langs=["Ada", "C", "C++"],
    extra=(
        "package Coverage is\n"
        '   for Excluded_Source_Files use ("main.cpp", "ada_helper.ads", '
        ' "ada_helper.adb");\n'
        "end Coverage;"
    ),
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--restricted-to-languages=Ada,C++"],
    extra_coverage_args=[
        "--annotate=xcov",
        "--restricted-to-languages=Ada,C++",
    ],
    trace_mode="src",
    tolerate_instrument_messages=".*",
    tolerate_coverage_messages=".*",
)

thistest.fail_if_no_match(
    "gnatcov instrument output",
    r"warning: All of the parts for the unit of interest ada_helper were"
    " excluded. Consider using --excluded-units.\n"
    r"warning: All of the parts for the unit of interest .*main.cpp were"
    " excluded. Consider using --excluded-units.\n",
    contents_of("instrument.log"),
)

thistest.fail_if_no_match(
    "gnatcov coverage output",
    "warning: no SID file found for unit main.cpp\n"
    "warning: no SID file found for unit ada_helper\n",
    contents_of("coverage.log"),
)
thistest.result()
