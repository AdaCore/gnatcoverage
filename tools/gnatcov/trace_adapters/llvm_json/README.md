Note: You'll need an LLVM toolchain installed at `${LLVM_INSTALL}`.

`${LLVM_INSTALL}/bin/llvm-*` commands should be in the PATH.

## Anod env setup

Required anod dependencies :
- `gnatall`
- `stable-libclang`
- `thirdparty-zlib` if the LLVM build from which you will take inputs was
    compiled with ZLIB enabled.

## Build

It is possible to build this tool with CMake or GPRbuild.

Both are wrapped in a Makefile to handle environment configuration.

### Build with GPRBUILD

You might want to adapt `LIBSTD_PATH` to the path of the `lib64` directory
installed by anod's gnatall.

### Build with Cmake

**Minimum required CMake version**: 3.0

The purpose of the CMake alternative is to generate a `compile_commands.json`
file that helps LSPs to find declarations.

