#
# Makefile to build/install the GNATcoverage documentation.
# This is a Sphinx based documentation.
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         = a4
BUILDDIR      = _build

PREFIX        = _install
INSTALLDIR    = $(PREFIX)/share/doc/gnatcoverage

# Documentation formats we know how to produce
DOCFORMATS=pdf html

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .

MKDIR=mkdir -p

.PHONY: clean all install

.NOTPARALLEL:

all: $(foreach fmt, $(DOCFORMATS), doc.$(fmt))

install: $(foreach fmt, $(DOCFORMATS), install.$(fmt))

clean:
	-rm -rf $(BUILDDIR) $(INSTALLDIR)

# -----------------------------------------------------------------
# Installing the doc

prepare-installdir.%:
	-rm -rf $(INSTALLDIR)/$*
	-$(MKDIR) $(INSTALLDIR)/$*

# Extract the master doc name from the sphinx conf.py script, where we
# expect to have a line like << master_doc = "bla" or 'bla' >> someplace.

MASTER_DOC=$(shell sed -n -e \
   "s/master_doc.*=[^'\"]*['\"]\([^'\"]*\).*/\1/p" conf.py)

# Make sure the PDF we install has a meaningful name and leave the html
# alone so internal hyperlinks to the toplevel page remain functional.

install.pdf: prepare-installdir.pdf
	cp $(BUILDDIR)/pdf/$(MASTER_DOC).pdf $(INSTALLDIR)/pdf/gnatcoverage.pdf

install.html: prepare-installdir.html
	rsync -a --delete $(BUILDDIR)/html/ $(INSTALLDIR)/html/

# -----------------------------------------------------------------
# Preparing image files for the doc

.PHONY: prepare-images

# Images need to be produced in two formats, pdf and png for
# inclusion by sphinx within pdf or html documents. They are better
# located in the same dir as the sources which include them.

# The only images we generate today are from graphviz  dot files

%.png: %.dot
	dot -T png -o $@ $<

%.pdf: %.dot
	dot -T pdf -o $@ $<

# gnatcov/ images:

gnatcov/multipath-bdd.png: gnatcov/multipath-bdd.dot
gnatcov/multipath-bdd.pdf: gnatcov/multipath-bdd.dot

gnatcov/consolidation.png: gnatcov/consolidation.dot
gnatcov/consolidation.pdf: gnatcov/consolidation.dot

prepare-images:: gnatcov/consolidation.png gnatcov/consolidation.pdf
prepare-images:: gnatcov/multipath-bdd.png gnatcov/multipath-bdd.pdf

# Images related to project facilities. There is actually a set of images,
# produced out of generated dot files. We use one of the binary results as
# a marker to determine whether we need to regenerate the whole set.

prepare-images:: gnatcov/Proot.pdf

gnatcov/Proot.pdf: gnatcov/gen_prjtrees.py gnatcov/prjtree.dot
	cd gnatcov && python gen_prjtrees.py


# -----------------------------------------------------------------
# Building the doc

# We need images in some parts of the doc and might not have the tools
# to build them in the nightly setups. We just keep binary versions in
# the repo and assume they are up to date in "prod" mode. Image files
# are actually sources for the Sphinx doc build process.

PREPARE_IMAGES=$(if $(filter prod, $(BUILD_MODE)),,prepare-images)

prepare-builddir.%: $(PREPARE_IMAGES)
	-rm -rf $(BUILDDIR)/$*
	-$(MKDIR) $(BUILDDIR)/$*

build.pdf: prepare-builddir.pdf
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/pdf
	make -C $(BUILDDIR)/pdf all-pdf LATEXOPTS="-interaction=nonstopmode"

build.html: prepare-builddir.html
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

doc.%: build.%
	@echo $@ done
