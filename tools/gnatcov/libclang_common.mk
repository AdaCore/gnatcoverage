#########################
# COMMON COMPILER FLAGS #
#########################

# The llvm-config command returns Windows-like path (with \\) on Windows.
# Storing them in a variable does not work, as every time the variable is
# expanded, it looses a \. We will convert the path to a POSIX-like path
# to avoid this.
#
# Note that for some mystic reason, cygpath (the available utility that is
# supposed to convert Windows path to POSIX-like paths) does not work on
# production machines. So we will use good old sed method.

LLVM_INCLUDEDIR=$(shell echo $$(llvm-config --includedir) | sed 's/\\/\//g')
LLVM_LIBDIR=$(shell echo $$(llvm-config --libdir) | sed 's/\\/\//g')

# The libclang driver library on Windows uses the system "version" API
CLANG_SYSTEM_LIBS_windows=-lversion
CLANG_SYSTEM_LIBS_linux=
CLANG_SYSTEM_LIBS=$(CLANG_SYSTEM_LIBS_$(HOST_OS))

CLANG_LIBS=\
	$(filter-out %.dll.a, $(wildcard $(LLVM_LIBDIR)/libclang*.a)) \
	$(CLANG_SYSTEM_LIBS)
CLANG_FLAGS=\
        -Wl,--start-group $(CLANG_LIBS) -Wl,--end-group
LLVM_FLAGS=\
        $(shell llvm-config --libs all)
LLVM_SYSTEM_LIBS=\
        $(shell llvm-config --system-libs)
LD_FLAGS=\
        $(shell llvm-config --ldflags) $(CLANG_FLAGS) $(LLVM_FLAGS) \
	$(LLVM_SYSTEM_LIBS) -static-libstdc++

CXXFLAGS=-I$(LLVM_INCLUDEDIR)
