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


.PHONY: help clean html pdf

clean.%:
	-rm -rf $(BUILDDIR)/$*

clean: clean.pdf clean.html
	-rm -rf $(BUILDDIR)/doctrees
	-rmdir $(BUILDDIR)

copy.pdf:
	-mkdir -p $(COPYDIR)
	cp $(BUILDDIR)/pdf/*.pdf $(COPYDIR)

copy.html:
	-mkdir -p $(COPYDIR)
	rsync -a --delete $(BUILDDIR)/html/ $(COPYDIR)

prepare:
	VERSION=$$(sed -n -e 's/.*Version.*"\(.*\)".*/\1/p' ../../version.ads); \
	perl -pi -e "s#^version =.*#version = \"$$VERSION\"#" conf.py

html: prepare
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html

pdf: prepare
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BUILDDIR)/pdf
	make -C $(BUILDDIR)/pdf all-pdf
