from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest


# In binary trace mode, we need to help gnatcov find the source for
# stacks_g.ads, as no debug info references it (debug info is the only source
# of absolute paths for source files).
TestCase().run(
    CovControl(covoptions="--source-search=../../src", auto_units=True)
    if thistest.options.trace_mode == "bin"
    else None
)
thistest.result()
