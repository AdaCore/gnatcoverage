# This makefile demonstrates how to use the integration makefile generated
# by GNATtest to obtain coverage reports on the units tested by GNATtest.
# The project consists of two units, Pkg1 and Pkg2, with Pkg2 depending on
# Pkg1.
#
# The goal is to only have coverage from unit tests for each unit count
# towards the coverage assesment of taht particular unit (i.e. unit tests on
# Pkg2 should only contribute to coverage for unit Pkg2, and not Pkg1).
#
# Tests are partially implemented in the "tests" directory, only tests for
# Pkg2 have been implemented so that coverage violation on Pkg1 should appear,
# if coverage for each unit is indeed assessed in isolation.

.PHONY: clean

clean:
	rm -rf obj/ harness/

# This rule shows how separate drivers can help avoid incidental coverage,
# by preventing calls to Pkg1 subprograms in Pkg2 testing from counting
# towards coverage for Pkg1.
gnattest-separate: clean
	gnattest -P prj.gpr --separate-drivers=unit && make -C harness/ coverage

# This rule uses a single monolithic driver and shows how Pkg1 ends up fully
# covered despite no having any tests written for it.
gnattest-monolithic: clean
	gnattest -P prj && make -C harness/ coverage