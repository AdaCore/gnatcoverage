# This example showcases the instrumentation and build of a project exercising
# SPARK features.
#
# We provide, under the ../support directory, a configuration pragma file that
# deactivates unsupported SPARK constructs. This file must be passed to -gnatec
# when building instrumented sources, to avoid running into SPARK constraints
# violations.

# The sequence used here illustrates the steps for a native environment,
# assuming a gnatcov runtime has been set up already.
#
# Cross configurations would require only the usual adjustments (build with
# --target/--RTS, probably use a more suitable gnatcov_rts, execute in the
# cross environment, ...). The "handling spark" specific part of the process
# remains the same.

all:
	gnatcov instrument -P spark_project.gpr --level=stmt+mcdc --dump-trigger=atexit
	echo $(CURDIR)
	gprbuild -P spark_project.gpr --implicit-with=gnatcov_rts_full --src-subdirs=gnatcov-instr -gnatec="$(CURDIR)/../support/instrument-spark.adc"
	GNATCOV_TRACE_FILE="main.srctrace" ./obj/main
	gnatcov coverage -P spark_project.gpr --level=stmt+mcdc --annotate=dhtml --output-dir=report main.srctrace

clean:
	rm -rf report main.srctrace obj
