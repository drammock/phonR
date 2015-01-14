# /usr/bin/python
hack = """
<script type="text/x-mathjax-config"> 
    MathJax.Hub.Config({ 
        "HTML-CSS": { scale: 80, linebreaks: { automatic: true } }, 
        SVG: { linebreaks: { automatic:true } }, 
        displayAlign: "left" });
</script>
"""

with open('phonR.html', 'r') as f:
	with open('index.html', 'w') as g:
		for line in f:
			if '/head' in line:
				g.write(hack)
			g.write(line)

