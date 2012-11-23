GNATMAKE=gnatmake
RM=rm -f
CP=cp -pf
LN=ln -sf
MKDIR=mkdir -p
RMDIR=-rmdir
MAKEINFO=makeinfo
exeext=

# Binary program name
PGMNAME=gnatcov

# Directory name in the install tree
DIRNAME=gnatcoverage

# Documentation formats to produce on "make doc"
DOCFORMATS=pdf html

# Base installation dir. Should be absolute pathname.
PREFIX=$(PWD)/install

############################
# Kind of HOST OS we're on #
############################

ifneq (,$(findstring win, ${OSTYPE}))
HOST_OS=windows
endif
ifneq (,$(findstring linux, ${OSTYPE}))
HOST_OS=linux
endif

#####################################################################
# Targets to build the core tool and the trace adapters, optionally #
#####################################################################

# We provide distinct targets for the two categories of artifacts so
# users get the core only by default in case they don't need the
# adapters for their host, e.g. when building for gnatemu targets.

# For example, a user who needs to build gnatcov on linux to perform
# ppc-elf coverage only doesn't need the valgrind adapter for native
# coverage. He might moreover be on a system where the adapter build
# would fail (e.g. missing header files), so we really shouldn't try
# to build these adapters by default.

# So ...
#
# make or make gnatcov
#
#   builds the core gnatcov tool only
#
# make all or make gnatcov trace-adapters
#
#   builds both the tool and the trace-adapters of
#   relevance for the host

# Determine the set of ADAPTER_SUBDIRS of relevance on our host OS

ADAPTER_SUBDIRS =
ifeq ($(HOST_OS), windows)
ADAPTER_SUBDIRS = trace_adapters/nexus
endif
ifeq ($(HOST_OS), linux)
ADAPTER_SUBDIRS = trace_adapters/valgrind
endif

.PHONY: default all trace-adapters $(PGMNAME) $(ADAPTER_SUBDIRS)

default: $(PGMNAME)

trace-adapters: $(ADAPTER_SUBDIRS)

all: $(PGMNAME) trace-adapters

$(PGMNAME):
	$(GNATMAKE) -Pgnatcov $(EXTRA_ADAFLAGS) $@

$(ADAPTER_SUBDIRS):
	$(MAKE) -C $@


#######################
# Internal facilities #
#######################

ppc_genopc:
	$(GNATMAKE) -Pgnatcov $(EXTRA_ADAFLAGS) $@

ppc_disopc.ads: ppc_genopc ppc_disopc.tmpl
	./ppc_genopc > $@

GNAT_SOURCE_DIR=../gnat_src
ADA_GEN_SUBDIR=gnat
MOVE_IF_CHANGE=mv -f

copy_gnat_src: force
	@echo "GNAT sources are now mirrored in the GNATCOV repository iteslf"

#################
# Documentation #
#################

# We need two kinds of targets for documentation:
# - One for developers that just builds the docs and leaves
#   everything in place (e.g. temporary sphinx build subdirs)
# - One for package production purposes that clears the
#   intermediate artifacts

# The 'doc' general target builds all the formats and cleans up the build
# trees afterwards. This is to be used for package construction.

# The 'doc.<format>' targets just builds a format and leaves artifacts
# around.

# In both cases the constructed user doc elements are copied
# to the doc/<format> subdir eventually.

UMAN_DOCDIR=doc/umanual

doc-build.%:
	make -C $(UMAN_DOCDIR) $*

doc-copy.%:
	make -C $(UMAN_DOCDIR) COPYDIR=$(shell pwd)/doc/$* copy.$*

.PHONY: doc

doc.%: doc-build.% doc-copy.%
	@echo $@ done

doc: $(foreach fmt, $(DOCFORMATS), doc.$(fmt)) clean-doc-build

###################
# General cleanup #
###################

.PHONY: clean-bin clean-doc-copy clean-doc-build clean

clean-bin:
	$(RM) *.o *.ali $(PGMNAME) b~*

clean-doc-copy.%:
	   $(RM) -r doc/$*

clean-doc-copy: $(foreach fmt, $(DOCFORMATS), clean-doc-copy.$(fmt))

clean-doc-build:
	make -C $(UMAN_DOCDIR) clean

clean: clean-bin clean-doc-build clean-doc-copy
	for dir in $(ADAPTER_SUBDIRS); do \
	  $(MAKE) -C $$dir clean; \
	done

################
# Installation #
################

.PHONY: install-bin install-examples install-doc install install-adapters

install-bin:
	$(MKDIR) $(PREFIX)/bin
	$(CP) $(PGMNAME)$(exeext) $(PREFIX)/bin

install-examples:
	$(MKDIR) $(PREFIX)/share/examples/$(DIRNAME)
	$(CP) -r examples/* $(PREFIX)/share/examples/$(DIRNAME)

install-doc.%:
	   $(MKDIR) $(PREFIX)/share/doc/$(DIRNAME)/$*
	   $(CP) -r doc/$*/* $(PREFIX)/share/doc/$(DIRNAME)/$*

install-doc: $(foreach fmt, $(DOCFORMATS), install-doc.$(fmt))

install: install-bin install-examples install-doc

install-adapters:
	for dir in $(ADAPTER_SUBDIRS); do \
	  $(MAKE) -C $$dir PREFIX=$(PREFIX) install || exit 1; \
	done

#############
# Packaging #
#############

# Prepare a to-be packaged distribution tree, accounting for a
# provided set of EXTRA_TOOLS to bundle in.

# Unix distribs are tar or zip versions of the three main items (bin, doc
# and examples), unpacked by users then installed with a doinstall script.
# We need to add this script, a README and an INSTALL file in this case.

# Windows distribs are packaged and installed by an nsis installer or alike,
# which has it's own extra info embedded and eventually installs just all what
# the distrib tree contains. We don't want any toplevel item there.

.PHONY: distrib-Linux-extra distrib-Windows_NT-extra distrib
.PHONY: install-extra-tools

# Where the provided set of $(EXTRA_TOOLS) should be installed
EXTRA_TOOLS_DIR=$(PREFIX)/libexec/gnatcoverage

install-extra-tools:
	$(MKDIR) $(EXTRA_TOOLS_DIR);
	for tool in $(EXTRA_TOOLS); do \
	  $(CP) $$tool $(EXTRA_TOOLS_DIR); \
	done

distrib-linux-extra:
	$(CP) README INSTALL $(PREFIX)
	sed -e "s/^version=.*/version=\"$$($(PGMNAME) --version)\"/" \
	    -e "s/^machine=.*/machine=\"$$(gcc -dumpmachine)\"/" \
        doinstall.tmplt > $(PREFIX)/doinstall
	chmod +x $(PREFIX)/doinstall

distrib-windows-extra:

distrib: install install-adapters install-extra-tools distrib-${HOST_OS}-extra

.PHONY: force

force:
