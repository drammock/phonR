# /bin/bash
Rscript -e "knitr::knit('phonR.Rmd')"   # phonR.Rmd > phonR.md
cd images
for i in *.pdf; do 
	inkscape -f "$i" -d 72 -b "white" -e "$(basename $i .pdf).png"
done
cd ..
python vignette_postprocessing.py       # phonR.md > index.md
Rscript -e "knitr::pandoc('index.md')"  # index.md > phonR.html
#python vignette_mathjax_resizer.py     # phonR.html > index.html
mv phonR.html index.html

# cleanup
rm phonR.html
rm *.pyc
rm index.md
rm phonR.md
