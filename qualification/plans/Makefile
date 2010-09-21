
all: check clean pdf

check:
	@if [ "$(PDFS)" = "" ]; then \
	  echo "no DOCS to make"; \
	fi

clean:
	rm -f *.bbl *.aux *.log *.dvi *.blg *.toc

pdf:
	pdflatex tqp.tex
	pdflatex tqp.tex
