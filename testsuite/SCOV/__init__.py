# ***************************************************************************
# ***                         SOURCE COVERAGE SERVICES                    ***
# ***************************************************************************

# Services aimed for source coverage testing activities.

# Test CONTEXT and CATEGORY
# -------------------------

# Each test is assigned a CATEGORY, which determines the set of coverage
# criteria it is designed to verify. Orthogonally, each test might be
# exercised in several possible coverage CONTEXTs, typically a target tool
# qualification level.

# The CONTEXT determines which --level argument we pass to xcov coverage.
# The CATEGORY determines the set of outputs we care about.

# For example, we will have categories for statement, decision or
# statement+mcdc testing purposes, which all translate into different sets of
# expectations on the results.

# For a do178 levelA qualification (CONTEXT), we will run all of these tests
# with --level=stmt+mcdc. This will typically produce mcdc related diagnostics
# in !mcdc oriented tests (CATEGORY), which we'll have to ignore.

# We expect the CONTEXT to designate a superset of the CATEGORY.

# Outputs of interest
# -------------------

# We care about two kinds of outputs:
# * the annotated sources produced by xcov --annotate=xcov, and
# * the violations list report produced by xcov --annotate=report
#
# We refer to them as the '=xcov' and the '=report' outputs respectively.

