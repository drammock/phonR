# /usr/bin/python
with open('phonR.md', 'r') as f:
	with open('index.md', 'w') as g:
		for line in f:
			#if '/head' in line:
			#	g.write('<link rel="stylesheet" type="text/css" href="phonR.css" />')
			#if '.pdf' in line and 'img src' in line:
			#	line = line.replace('.pdf', '.png')
			if '.pdf' in line and '<img src=' in line:
				line = line.replace('.pdf', '.png')
			g.write(line)
