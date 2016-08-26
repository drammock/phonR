all: html pdf clean

.PHONY: clean purge

html:
	Rscript -e "knitr::knit('phonR.Rmd')"   # phonR.Rmd > phonR.md
	python change-image-extensions.py       # phonR.md > index.md
	cd images; for pdffile in *.pdf ; do \
	pdftoppm -f 1 -singlefile -rx 72 -ry 72 -png $${pdffile} \
	$$(basename $${pdffile} .pdf) ; done
	Rscript -e "knitr::pandoc('index.md')"  # index.md > phonR.html
	#python resize-mathjax.py               # phonR.html > index.html
	mv phonR.html index.html

pdf:
	python add-pandoc-pdf-header-args.py  # phonR.Rmd > tex.Rmd
	Rscript -e "knitr::knit('tex.Rmd')"   # tex.Rmd > tex.md
	python markdownify-image-tags.py      # tex.md > phonR.md
	python make-refs-hanging-indent.py    # phonR.md > phonR.md
	# run pandoc manually: knitr bugs in display math in tables
	pandoc -s --no-tex-ligatures --template=vignette_template.tex \
	--latex-engine=xelatex --highlight-style=pygments \
	--csl=phonR.csl --bibliography=phonR.bib -V geometry:margin=1in \
	-o tex.tex phonR.md
	# typeset
	xelatex --shell-escape tex.tex
	xelatex --shell-escape tex.tex
	mv tex.pdf phonR.pdf

purge: clean
	rm -f cache/* images/*

clean:
	rm -f index.md phonR.md phonR.html tex.* *.pyc
