GNATMAKE=gnatmake
PREFIX=install

include ../../host.mk

.PHONY: force

ARCH=32

# Files for libexec/gnatcoverage include binary executables
# (which need to be built first), and a python script.

EXENAMES=nexus_trace_gen isys_drv
PYPROG=isys_trace.py

# Files for share/gnatcoverage are the winIdea project files
WINIDEA_PROJ_FILES=min5554.trd min5554.xjrf min5554.xqrf \
                   min5634.trd min5634.xjrf min5634.xqrf

# Directory name in the install tree
DIRNAME=gnatcoverage

DOCFORMATS=pdf html

all: $(EXENAMES)

$(EXENAMES): force
	$(GNATMAKE) -Pnexus -XARCH=$(ARCH) $(EXTRA_ADAFLAGS) $@

GNAT_SOURCE_DIR=../gnat_src
ADA_GEN_SUBDIR=gnat
MOVE_IF_CHANGE=mv -f

copy_gnat_src: force
	@echo "GNAT sources are now mirrored in the GNATCOV repository iteslf"


###################
# General cleanup #
###################

.PHONY: clean

clean:
	$(RM) *.o *.ali $(EXENAMES) b~* b__*

################
# Installation #
################

.PHONY: install-libexec install-share install

install-libexec:
	$(MKDIR) $(PREFIX)/libexec/$(DIRNAME)
	$(CP) $(addsuffix $(exeext),$(EXENAMES)) \
          $(PYPROG) $(PREFIX)/libexec/$(DIRNAME)

install-share:
	$(MKDIR) $(PREFIX)/share/$(DIRNAME)
	$(CP) $(WINIDEA_PROJ_FILES) $(PREFIX)/share/$(DIRNAME)

install: install-libexec install-share
