RUSTFLAGS=-Cinstrument-coverage

ifeq (${DEBUG}, 1)
PROFILE=debug
else
PROFILE=release
endif

ifeq (${MCDC}, 1)
EXTRA_RUSTFLAGS=-Ccoverage-options=mcdc
LEVEL=stmt+mcdc
else
LEVEL=stmt
endif

PROFRAW=trace.profraw

.PHONY: build run coverage clean

default: coverage

build:
	RUSTFLAGS="${RUSTFLAGS} ${EXTRA_RUSTFLAGS}" cargo build --${PROFILE}

run: build
	LLVM_PROFILE_FILE=${PROFRAW} target/${PROFILE}/rust_cov

coverage: run
	gnatcov coverage --level=${LEVEL} -areport --exec target/${PROFILE}/rust_cov ${PROFRAW}

clean:
	${RM} ${PROFRAW}
	${RM} -r stats
	cargo clean
