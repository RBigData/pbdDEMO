#!/bin/sh

rm *.aux *.bbl *.blg *.log *.out *.toc *.dvi
pdflatex pbdDEMO-guide.Rnw
bibtex pbdDEMO-guide
pdflatex pbdDEMO-guide.Rnw
pdflatex pbdDEMO-guide.Rnw
pdflatex pbdDEMO-guide.Rnw
rm *.aux *.bbl *.blg *.log *.out *.toc *.dvi
