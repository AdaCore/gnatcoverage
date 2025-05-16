PREFIX=install
PARALLEL_BUILD=0
include ../../host.mk

LLVM_CXXFLAGS := $(shell llvm-config --cxxflags)
LLVM_LDFLAGS := $(shell llvm-config --ldflags)
LLVM_LIBS := $(shell llvm-config --libs --system-libs)

# Set this to the right path for you
GCC_TOOLCHAIN := "$(dir $(shell which gnat)).."
LIBSTD_PATH := "$GCC_TOOLCHAIN/lib64/"

EXENAME=gnatcov_llvm_exporter
DIRNAME=gnatcoverage

GPRBUILD_FLAGS := \
			-cargs:c++ $(LLVM_CXXFLAGS) -g \
			-largs \
				-L $(LIBSTD_PATH) \
				$(LLVM_LDFLAGS) \
				$(LLVM_LIBS)

.PHONY: force

bin: force
	gprbuild -j$(PARALLEL_BUILD) $(GPRBUILD_FLAGS)

cmake:
	mkdir -p build
	cmake \
		-B build \
		-DCMAKE_C_COMPILER=gcc \
		-DCMAKE_CXX_COMPILER=g++ \
		-DCMAKE_CXX_FLAGS="$(LLVM_CXXFLAGS)" \
		-DCMAKE_EXE_LINKER_FLAGS="-L $(LIBSTD_PATH)"
	cd build && make

.PHONY: clean

clean:
	gprclean
	$(RM) -rf obj
	$(RM) -rf build

.PHONY: install

install:
	$(MKDIR) $(PREFIX)/libexec/$(DIRNAME)
	$(CP) $(EXENAME) $(PREFIX)/libexec/$(DIRNAME)
