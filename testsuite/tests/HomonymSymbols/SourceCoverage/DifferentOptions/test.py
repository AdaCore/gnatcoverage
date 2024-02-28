"""
Check that gnatcov tries and fails to consolidate in source coverage two "f"
symbols, both with debug information and from the same compilation unit, but
compiled with different options.
"""

from OCOV.tc import TestCase
from SUITE.context import thistest
from SUITE.cutils import Wdir


test_drivers = {"test_1": {"not.c": ["-O0"]}, "test_2": {"not.c": ["-O1"]}}
coverage_expectations = {
    # There is no object coverage expectation. This entry is to add "f" to the
    # routines to consolidate.
    "f": None,
}

# SCOs are the same for each project: not.gli
wd = Wdir("tmp_")
with open("scos.list", "w") as f:
    for sco_f in ["not.gli"]:
        f.write("test_1-obj/{}\n".format(sco_f))
wd.to_homedir()

tc = TestCase(
    test_drivers,
    coverage_expectations,
    extra_sourcedirs=["../../../src"],
    level="stmt",
    annotate="xcov",
    extra_xcov_args=["--scos=@scos.list"],
)
thistest.fail_if(
    tc.run(register_failure=False),
    '"gnatcov coverage" was supposed to fail, but it did not',
)
thistest.result()
