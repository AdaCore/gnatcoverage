# Example demonstrating how to instrument a project for a cross configuration,
# with a restricted runtime. By default the example runs on a native target, as
# the binary execution step differs for each target.

RTS?=light
TARGET?=

# target & runtime options to be passed to all gnatcov & gprbuild commands
GPR_ARGS=

ifneq ($(TARGET),)
GPR_ARGS = --target=$(TARGET)
endif

ifneq ($(RTS),)
GPR_ARGS := $(GPR_ARGS) --RTS=$(RTS)
endif

RTS_DIR = rts_install

.PHONY: instrument build run coverage

default: coverage

# Only needs to be called once, can be re-used across multiple GNATcov workflows
$(RTS_DIR)/share/gpr/gnatcov_rts.gpr:
	gnatcov setup $(GPR_ARGS) --prefix=$(RTS_DIR)

insturment: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
instrument: $(RTS_DIR)/share/gpr/gnatcov_rts.gpr
	@echo 'export GPR_PROJECT_PATH=$$(pwd)/$(RTS_DIR)/share/gpr:$$GPR_PROJECT_PATH'
	gnatcov instrument -P monitor.gpr $(GPR_ARGS) --level=stmt+decision \
	--dump-channel=base64-stdout --dump-trigger=main-end

build: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
build: instrument
	gprbuild -f -p -P monitor.gpr $(GPR_ARGS) --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

ifeq ($(TARGET),)
run: build
	./obj/monitor > monitor.stdout
	@cat monitor.stdout
else
run: build
	@echo I don't know how to execute a binary for $(TARGET), please modify line 40 of the Makefile,\n\
	the output of the program should be captured in the monitor.stdout file. && false
endif

coverage: run
	gnatcov extract-base64-trace monitor.stdout monitor.srctrace
	gnatcov coverage -P monitor.gpr $(GPR_ARGS) --level=stmt+decision --annotate=xcov monitor.srctrace
	cat obj/monitor.adb.xcov

clean:
	rm -rf obj $(RTS_DIR) monitor.srctrace monitor.stdout
