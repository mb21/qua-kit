all: pdf

pdf:
	latexmk -pdf src/specification -synctex=1 -outdir=bin/
	latexmk -pdf src/specification -synctex=1 -outdir=bin/
	latexmk -pdf src/specification -synctex=1 -outdir=bin/

clean:
	rm -r -f bin/*.aux bin/*.log bin/*.bbl\
		bin/*.blg bin/*.toc bin/*.lof bin/*.lot\
		bin/*.out bin/*.fls bin/*.synctex.gz bin/*.fdb_latexmk
