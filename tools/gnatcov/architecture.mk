DISTRIB_ARCH=$(shell uname -m)

#
# Translation from `uname -m` to a suitable value for valgrind
#
VALGRIND_ARCH_x86    = x86
VALGRIND_ARCH_i386   = x86
VALGRIND_ARCH_i486   = x86
VALGRIND_ARCH_i586   = x86
VALGRIND_ARCH_i686   = x86
VALGRIND_ARCH_x86_64 = amd64
VALGRIND_ARCH_amd64  = amd64

VALGRIND_ARCH=$(VALGRIND_ARCH_$(DISTRIB_ARCH))

#
# Translation from `uname -m` to 32 or 64 (bits)
#
BITS_ARCH_x86    = 32
BITS_ARCH_i386   = 32
BITS_ARCH_i486   = 32
BITS_ARCH_i586   = 32
BITS_ARCH_i686   = 32
BITS_ARCH_x86_64 = 64
BITS_ARCH_amd64  = 64

BITS_ARCH=$(BITS_ARCH_$(DISTRIB_ARCH))
