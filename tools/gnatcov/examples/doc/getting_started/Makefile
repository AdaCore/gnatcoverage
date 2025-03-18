# Simple project demonstrating the usual GNATcoverage workflow.
# See section "Getting Started" in GNATcoverage's user manual for more
# details.

RTS_DIR = rts_install

.PHONY: instrument build run coverage

default: coverage

# Only needs to be called once, can be re-used across multiple GNATcov workflows
$(RTS_DIR)/share/gpr/gnatcov_rts.gpr:
	gnatcov setup --prefix=$(RTS_DIR)

instrument: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
instrument: $(RTS_DIR)/share/gpr/gnatcov_rts.gpr
	@echo 'export GPR_PROJECT_PATH=$$(pwd)/$(RTS_DIR)/share/gpr:$$GPR_PROJECT_PATH'
	gnatcov instrument -P tests.gpr --level=stmt

build: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
build: instrument
	gprbuild -f -p -P tests.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

run: build
	./obj-tests/test_inc

coverage: run
	gnatcov coverage -P tests.gpr --level=stmt --annotate=xcov test_inc*.srctrace
	cat obj-tests/ops.adb.xcov

clean:
	rm -rf obj-* $(RTS_DIR) test_inc*.srctrace
