# This showcases how to use the integrated instrumentation mode with a Makefile
# or a CMake build system. For more details see subsections "A simple Makefile
# example" and "A simple CMake example" of section "Producing source traces
# with integrated instrumentation".

RTS_DIR = rts_install
ARTIFACTS_DIR = gnatcov_artifacts

.PHONY: setup-instr build run coverage clean

# Set this variable to either Makefile or CMake to select the desired build system
BUILD_SYSTEM ?= Makefile

default: coverage

# Only needs to be called once, can be re-used across multiple GNATcov workflows
$(RTS_DIR)/share/gpr/gnatcov_rts.gpr:
	gnatcov setup --prefix=$(RTS_DIR)

setup-instr: export GPR_PROJECT_PATH := $(shell pwd)/$(RTS_DIR)/share/gpr:$(GPR_PROJECT_PATH)
setup-instr: $(RTS_DIR)/share/gpr/gnatcov_rts.gpr
	gnatcov setup-integration --files=hello_world/main.cpp --compilers=g++ \
	--level=stmt --output-dir=$(ARTIFACTS_DIR)

build-Makefile: export PATH:=$(shell pwd)/$(ARTIFACTS_DIR):$(PATH)
build-Makefile: setup-instr
	make -C hello_world test
	cp hello_world/test .

build-CMake: setup-instr
	cmake hello_world -B cmake_build -DCMAKE_CXX_COMPILER=$(shell pwd)/$(ARTIFACTS_DIR)/g++
	cmake --build cmake_build
	cp cmake_build/test .

run: build-$(BUILD_SYSTEM)
	./test

coverage: run
	gnatcov coverage --sid $(ARTIFACTS_DIR)/main.cpp.sid --level=stmt \
	--annotate=xcov *.srctrace
	cat main.cpp.xcov

clean:
	rm -rf $(ARTIFACTS_DIR) $(RTS_DIR) main*.srctrace hello_world/main.o \
	hello_world/test stats *.xcov  CMakeFiles CMakeCache.txt cmake_* test
