LLVM_CXXFLAGS := $(shell llvm-config --cxxflags)
LLVM_LDFLAGS := $(shell llvm-config --ldflags)
LLVM_LIBS := $(shell llvm-config --libs --system-libs)

# Set this to the right path for you.
LIBSTD_PATH := "$(dir $(shell which gnat))../lib64/"

GPRBUILD_FLAGS := \
			-cargs:c++ ${LLVM_CXXFLAGS} -g \
			-largs \
				--verbose \
				-fuse-ld=lld \
				-L ${LIBSTD_PATH} \
				${LLVM_LDFLAGS} \
				${LLVM_LIBS}

gprbuild:
	@gprbuild -v ${GPRBUILD_FLAGS}

cmake:
	mkdir -p build
	cmake \
		-B build \
		-DCMAKE_C_COMPILER=clang \
		-DCMAKE_CXX_COMPILER=clang++ \
		-DCMAKE_CXX_FLAGS="${LLVM_CXXFLAGS}" \
		-DCMAKE_EXE_LINKER_FLAGS="-L ${LIBSTD_PATH}"
	cd build && make

.PHONY: clean

clean:
	gprclean
	${RM} -rf obj
	${RM} -rf build
