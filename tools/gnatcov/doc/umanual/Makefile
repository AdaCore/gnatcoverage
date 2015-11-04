# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         = a4
BUILDDIR      = _build
COPYDIR       = _copy

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) .


.PHONY: help clean html pdf all

all: html pdf

clean.%:
	-rm -rf $(BUILDDIR)/$*

clean: clean.pdf clean.html
	-rm -rf $(BUILDDIR)/doctrees
	-rmdir $(BUILDDIR)

copy.pdf:
# The doc build constructs a few intermediate pdfs on the fly for
# figures and we want the master doc only at the end. Extract the
# name from the sphinx conf.py script, where we expect to have a
# line like << master_doc = "gnatcov" or 'gnatcov' >> someplace.
	-mkdir -p $(COPYDIR)
	cp $(BUILDDIR)/pdf/$(shell sed -n -e \
               "s/master_doc.*=[^'\"]*['\"]\([^'\"]*\).*/\1/p" conf.py).pdf \
           $(COPYDIR)

copy.html:
	-mkdir -p $(COPYDIR)
	rsync -a --delete $(BUILDDIR)/html/ $(COPYDIR)

# Images need to be produced in two formats, pdf and png for
# inclusion by sphinx within pdf or html documents.

%.png: %.dot
	dot -T png -o $@ $<

%.pdf: %.dot
	dot -T pdf -o $@ $<

multipath-bdd.png: multipath-bdd.dot
multipath-bdd.pdf: multipath-bdd.dot

consolidation.png: consolidation.dot
consolidation.pdf: consolidation.dot

prepare-images:: consolidation.png consolidation.pdf
prepare-images:: multipath-bdd.png multipath-bdd.pdf

# Images related to project facilities. There is actually a set of images,
# produced out of generated dot files. We use one of the binary results as
# a marker to determine whether we need to regenerate the whole set.

prepare-images:: Proot.pdf

Proot.pdf: gen_prjtrees.py prjtree.dot 
	python gen_prjtrees.py

# Production mode needs to assume that the binary prerequisites
# are up to date. We can't rely on timestamps for this.

PREPARE_IMAGES=$(if $(filter prod, $(BUILD_MODE)),,prepare-images)

stamp-prepare: $(PREPARE_IMAGES)
	touch stamp-prepare

html: stamp-prepare
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

pdf: stamp-prepare
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/pdf
	make -C $(BUILDDIR)/pdf all-pdf LATEXOPTS="-interaction=nonstopmode"
