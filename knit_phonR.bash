#! /bin/bash
# clear cache if desired
if [[ $* == *--cache* ]]; then
	rm cache/*
fi
# knit markdown
Rscript -e "knitr::knit('phonR.Rmd')"   # phonR.Rmd > phonR.md
# regen images
if [[ $* == *--plot* ]]; then
	cd images
	for i in *.pdf; do 
		inkscape -f "$i" -d 72 -b "white" -e "$(basename $i .pdf).png"
	done
	cd ..
fi
# Knit HTML
python vignette_postprocessing_html.py  # phonR.md > index.md
Rscript -e "knitr::pandoc('index.md')"  # index.md > phonR.html
#python vignette_mathjax_resizer.py     # phonR.html > index.html
mv phonR.html index.html

# Knit PDF
# a script to put in proper pandoc header info
python vignette_preprocessing_latex.py      # phonR.Rmd > phonR_tex.Rmd
#Rscript -e "rmarkdown::render('phonR_tex.Rmd')"   # phonR_tex.Rmd > phonR_tex.md
Rscript -e "knitr::knit('phonR_tex.Rmd')"   # phonR_tex.Rmd > phonR_tex.md
pandoc --highlight-style=pygments --csl=phonR.csl --bibliography=phonR.bib -o phonR.html phonR_tex.md
pandoc -s --template=vignette_template.tex --latex-engine=xelatex --no-tex-ligatures -V geometry:margin=1in -o phonR.pdf phonR.html
# run pandoc manually, due to bugs in knitr's handling of display math in tables
#pandoc -s --template=vignette_template.tex --latex-engine=xelatex --highlight-style=pygments --csl=phonR.csl --bibliography=phonR.bib --no-tex-ligatures -V geometry:margin=1in -o phonR.tex phonR_tex.md
#python vignette_postprocessing_latex.py      # phonR.tex > phonR_cleaned.tex
#xelatex --shell-escape phonR_cleaned.tex
#xelatex --shell-escape phonR_cleaned.tex

# cleanup
rm *.pyc
rm index.md phonR.md
rm phonR_tex.*
rm phonR.html
