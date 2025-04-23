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

# The error message for exceeding the path limit isn't the same on Windows
# and on linux.

thistest.fail_if_no_match(
    what="Missing or unexpected error message",
    regexp=(
        (
            r".*/gnatcov\.exe: Could not create the buffer unit for "
            ".*overly_long_capitalized_c_unit_that_should_not_triple_in_size"
            '_to_avoid_path_name_limit.c: invalid path name "gcvrt_b_z.*'
        )
        if "windows" in thistest.env.host.platform
        else r".*/gnatcov: cannot open .*/tests"
        r"/92-long_slugs/tmp_/obj/gen-gnatcov-instr/gcvrt_b_z.*\.c"
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

# We normalize the case of files on Windows, as such the report files won't
# have the same case as the original filename.

check_xcov_reports(
    "obj", {"main.c.xcov": {"+": {6}}, f"{unit_name}.c.xcov": {"+": {6}}}
)

thistest.result()
