# /usr/bin/python
header = '''<!-- pandoc
output:
  pdf_document:
    s:
    t: latex
    o: phonR.pdf
    latex-engine: xelatex
    template: vignette_template.tex
    V: geometry:margin=1in
    bibliography: phonR.bib
    csl: phonR.csl
    no-tex-ligatures:
'''

with open('phonR.Rmd', 'r') as f:
	with open('phonR_tex.Rmd', 'w') as g:
		pandoc_header = False
		header_written = False
		for line in f:
			if '<!--pandoc' in line:
				pandoc_header = True
			if pandoc_header and '-->' in line:
				pandoc_header = False
			if pandoc_header:
				if not header_written:
					g.write(header)
					header_written = True
			else:
				g.write(line)
