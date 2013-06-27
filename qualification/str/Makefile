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

.PHONY: clean html latexpdf

all: html

clean:
	-rm -rf $(BLDDIR)/* $(SRCDIR)/*.rst

html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BLDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BLDDIR)/html."

latexpdf:
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) $(BLDDIR)/latexpdf
	@echo "Running LaTeX files through pdflatex..."
	make -C $(BLDDIR)/latexpdf all-pdf
	@echo "pdflatex finished; the PDF files are in $(BLDDIR)/latex."
