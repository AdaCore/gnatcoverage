# Makefile for Sphinx documentation
#

# You can set these variables from the command line.
SPHINXOPTS    = -c .
SPHINXBUILD   = sphinx-build
PAPER         = a4
BLDDIR      = build
SRCDIR      = source

# Internal variables.
ALLSPHINXOPTS   = -d $(BLDDIR)/doctrees $(SPHINXOPTS) $(SRCDIR)

# sphinx-build uses python modules of its own and we're possibly
# executed in a context which also needs a python distrib of its
# own. Arrange to launch sphinx-build with a PATH temporarily
# augmented with the python distrib that comes with it.
SPHINXDIR=$$(dirname $$(which $(SPHINXBUILD)))

.PHONY: clean html latexpdf

all: html

clean:
	-rm -rf $(BLDDIR)/* $(SRCDIR)/*.rst

html:
	PATH=$(SPHINXDIR):$$PATH; \
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BLDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BLDDIR)/html."

latexpdf:
	PATH=$(SPHINXDIR):$$PATH; \
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BLDDIR)/latexpdf
	@echo "Running LaTeX files through pdflatex..."
	make -C $(BLDDIR)/latexpdf all-pdf
	@echo "pdflatex finished; the PDF files are in $(BLDDIR)/latex."
