# This example showcases how to exclude testing subunits (a.k.a. separates)
# from coverage analysis, to be able to combine the coverage on a single unit,
# of multiple test subprograms.
#
# More information is available in section
# "Conveying source files to ignore / handling Ada subunits" in the
# GNATcoverage user's manual.

RTS_DIR = rts_install

default: coverage

# Only needs to be called once, can be re-used across multiple GNATcov workflows
$(RTS_DIR)/share/gpr/gnatcov_rts.gpr:
	gnatcov setup --prefix=$(RTS_DIR)

instrument-%: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
instrument-%: $(RTS_DIR)/share/gpr/gnatcov_rts.gpr
	@echo 'export GPR_PROJECT_PATH=$$(pwd)/$(RTS_DIR)/share/gpr:$$GPR_PROJECT_PATH'
	gnatcov instrument -P p.gpr --level=stmt+decision -XTEST="$*" \
	--ignore-source-files=@ignore-list.txt

build-%: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
build-%: instrument-%
	gprbuild -f -p -P p.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr -XTEST="$*"

run-%: build-%
	GNATCOV_TRACE_FILE=$*.srctrace ./obj/run_all

coverage: run-test1 run-test2
	gnatcov coverage -P p.gpr --level=stmt+decision --annotate=xcov \
	-XTEST="test1" --units=data_processing test1.srctrace test2.srctrace
	cat obj/data_processing.adb.xcov

clean:
	rm -rf obj/ $(RTS_DIR) *.srctrace
