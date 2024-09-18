# This example showcases the instrumentation and build of a project exercising
# SPARK features.
#
# We provide, under the ../support directory, a configuration pragma file that
# deactivates unsupported SPARK constructs. This file must be passed to -gnatec
# when building instrumented sources, to avoid running into SPARK constraints
# violations.

TESTS=main
XCOVLEVEL=stmt+mcdc
EXTRA_CARGS=-gnatec="$(CURDIR)/../support/instrument-spark.adc"

include ../support/Makefile.examples
