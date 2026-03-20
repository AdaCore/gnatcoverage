"""
Test that the output of the -M family of flags of the GCC preprocessor is not
clobbered with references to instrumentation artifacts.
"""

import os
import os.path

from SUITE.cutils import contents_of, Wdir
from SUITE.integrated_instr_utils import RunCompiler, setup_integration
from SUITE.tutils import thistest

Wdir("tmp_")

baseline_dep_file = "test.dep.expected"
baseline_wf = RunCompiler(
    switches=["../test.c", "-MM", "-MF", baseline_dep_file]
)
baseline_wf.build(env=dict(os.environ))

# Use the gnatcov wrapper to produce a dep file
dep_file = "test.dep.actual"
wrapper_wf = RunCompiler(switches=["../test.c", "-MM", "-MF", dep_file])
env = setup_integration(files_of_interest=["../test.c", "../special_0.h"])
wrapper_wf.build(env=env)

# Check the actual result against the baseline
thistest.fail_if_not_equal(
    what="Difference in generated dep file",
    expected=contents_of(baseline_dep_file),
    actual=contents_of(dep_file),
)

thistest.result()
