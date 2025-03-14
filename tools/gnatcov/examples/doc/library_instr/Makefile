# Project demonstrating how to instrument and get coverage over a library, and
# distribute the instrumented library independently, leveraging the
# Externally_Built GPR attribute.
#
# See paragraph "Example use cases" of the
# "Producing source traces with gnatcov instrument" section for more details

RTS_DIR = rts_install

.PHONY: instrument build run coverage

default: coverage

# Only needs to be called once, can be re-used across multiple GNATcov workflows
$(RTS_DIR)/share/gpr/gnatcov_rts.gpr:
	gnatcov setup --prefix=$(RTS_DIR)

instrument-lib: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
instrument-lib: $(RTS_DIR)/share/gpr/gnatcov_rts.gpr
	@echo 'export GPR_PROJECT_PATH=$$(pwd)/$(RTS_DIR)/share/gpr:$$GPR_PROJECT_PATH'
	gnatcov instrument -P code.gpr --level=stmt -XCODE_LIBMODE=instrument

build-lib: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
build-lib: instrument
	gprbuild -f -p -P code.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr \
	-XCODE_LIBMODE=build

instrument-test: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
instrument-test: instrument-lib
	gnatcov instrument -P tests.gpr --level=stmt --externally-built-projects --dump-trigger=atexit

build-test: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
build-test: instrument-test build-lib
	gprbuild -P tests.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

run: build-test
	./obj-tests/test_inc

coverage: run
	gnatcov coverage -P tests.gpr --level=stmt --annotate=xcov test_inc*.srctrace --externally-built-projects
	cat obj-tests/ops.adb.xcov

clean:
	rm -rf obj-* $(RTS_DIR) test_inc*.srctrace lib-*
