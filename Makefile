all: pdf

pdf:
	pdflatex specification
	pdflatex specification
	pdflatex specification
	
clean:
	rm -r -f *.aux *.log *.bbl *.blg *.toc *.lof *.lot *.out
