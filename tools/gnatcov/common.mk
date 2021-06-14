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

CLANG_FLAGS=\
        -Wl,--start-group $(LLVM_LIBDIR)/libclang*.a -Wl,--end-group
LLVM_FLAGS=\
        $(shell llvm-config --libs all)
SYSTEM_LIBS=\
        $(shell llvm-config --system-libs)
LD_FLAGS=\
        $(shell llvm-config --ldflags) $(CLANG_FLAGS) $(LLVM_FLAGS) $(SYSTEM_LIBS)

CXXFLAGS=-I$(LLVM_INCLUDEDIR)
