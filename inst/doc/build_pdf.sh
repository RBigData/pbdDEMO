#!/bin/sh

rm *.aux *.bbl *.blg *.log *.out *.toc *.idx *.lof *.lot *.ind *.ilg
pdflatex pbdDEMO-guide.Rnw
bibtex pbdDEMO-guide
#pdflatex pbdDEMO-guide.Rnw
#makeindex pbdDEMO-guide.idx
pdflatex pbdDEMO-guide.Rnw
pdflatex pbdDEMO-guide.Rnw
rm *.aux *.bbl *.blg *.log *.out *.toc *.idx *.lof *.lot *.ind *.ilg

Rscript -e "tools::compactPDF('./pbdDEMO-guide.pdf',gs_quality='ebook')"
