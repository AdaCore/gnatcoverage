# This subdirectory provides an example use of the gnatcov xml output. It
# leverages the common "build" and "trace" steps and bypasses the common
# "analyze" target to trigger the production of an xml output instead. It also
# requests the production of a text report, and performs a postprocessing step
# on the xml to illustrate the use of a python script to extract violations
# out of the xml matching those found in the text report.

# The xcov.py python code provides the start of what could become a more
# general python API to query gnatcov assessments.

TESTS=test_services
XCOVLEVEL=stmt+decision

local-all: build trace
local-all: out-test_services/index.xml out-test_services/report.txt
local-all: postprocess

include ../support/Makefile.examples


postprocess: announce-postprocess
postprocess: force
	cd out-test_services && ../xcov-report.py index.xml
