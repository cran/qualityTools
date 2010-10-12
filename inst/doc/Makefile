all:	pdf clean

pdf: 	qualityTools.tex
	pdflatex qualityTools
	bibtex qualityTools
	pdflatex qualityTools
	pdflatex qualityTools
	pdflatex qualityTools

clean:
	rm -f *.aux *.eps *.log *.out *.tex *.toc *.bbl
	rm -f Rplots.ps qualityTools-*