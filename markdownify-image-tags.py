#!/usr/bin/python
import xml.etree.ElementTree as ET

with open('tex.md', 'r') as f:
	with open('phonR.md', 'w') as g:
		for line in f:
			if '<img src=' in line:
				foo = ET.fromstring(line)
				alt = foo.get('alt')
				src = foo.get('src')
				title = foo.get('title')
				newline = u'![{}]({} "{}")'.format(alt, src, title)
				start = line.index('<img src=')
				end = line.index('/>') + 2
				line = (line[:start] + newline + line[end:]).encode('UTF-8')
			g.write(line)
