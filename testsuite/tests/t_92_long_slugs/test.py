"""
Check that gnatcov-generated units have reasonably short filenames.
Gnatcov used to generate slugs that could take up to three characters to
represent a single one for the original unit name, thus generating overly long
unit names.
"""

from SCOV.minicheck import (
    build_run_and_coverage,
    check_xcov_reports,
    xcov_instrument,
)
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor

Wdir("tmp_")

prj_switches = GPRswitches(gprfor(srcdirs=[".."], mains=["main.c"]))

# First, check that using the legacy slugs produces paths that exceed the path
# limit.

log = "slug_instr.log"
p = xcov_instrument(
    gprsw=prj_switches,
    covlevel="stmt",
    extra_args=["--full-slugs"],
    register_failure=False,
    out=log,
)
thistest.fail_if(p.status == 0)

# Exceeding the path limit is reported when gnatcov attempts to create the
# buffer unit file.

thistest.fail_if_no_match(
    what="Missing or unexpected error message",
    regexp=(
        r"gnatcov: cannot open .*/tests"
        r"/t_92_long_slugs/tmp_/obj/gen-gnatcov-instr/gcvrt_b_z.*\.c"
    ),
    actual=contents_of(log).replace("\\", "/"),
)

# Now check that the buffer units with hashes in their names work ok
build_run_and_coverage(
    gprsw=prj_switches,
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
)

unit_name = (
    "OVERLY_LONG_CAPITALIZED_C_UNIT_THAT_SHOULD_NOT_TRIPLE_IN_SIZE"
    "_TO_AVOID_PATH_NAME_LIMIT"
)

check_xcov_reports(
    "obj", {"main.c.xcov": {"+": {6}}, f"{unit_name}.c.xcov": {"+": {6}}}
)

thistest.result()
