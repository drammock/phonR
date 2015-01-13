# /bin/bash

#Rscript -e "knitr::knit2html('phonR.Rmd', options=c('use_xhtml', 'smartypants', 'mathjax', 'highlight_code'))"
Rscript -e "knitr::knit('phonR.Rmd')"
cd images
for i in *.pdf; do 
	inkscape -f "$i" -d 72 -b "white" -e "$(basename $i .pdf).png"
done
cd ..
python vignette_postprocessing.py
#pandoc -s phonR-png.md -c phonR.css -o index.html 
#pandoc -s --latex-engine=xelatex --template=vignette_template.tex phonR.md -o phonR.pdf
 
