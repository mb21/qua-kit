all: assets pdf

assets:
	latexmk -pdf src/assets/binary-message-format -synctex=1 -outdir=bin/assets

pdf:
	latexmk -pdf src/specification -synctex=1 -outdir=bin

clean:
	rm -r -f bin/*.aux bin/*.log bin/*.bbl\
		bin/*.blg bin/*.toc bin/*.lof bin/*.lot\
		bin/*.out bin/*.fls bin/*.synctex.gz bin/*.fdb_latexmk

	rm -r -f bin/*/*.aux bin/*/*.log bin/*/*.bbl\
		bin/*.blg bin/*/*.toc bin/*/*.lof bin/*/*.lot\
		bin/*/*.out bin/*/*.fls bin/*/*.synctex.gz bin/*/*.fdb_latexmk
