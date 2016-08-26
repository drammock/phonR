#!/usr/bin/python
indent = """
\setlength{\parindent}{-2em}
\setlength{\leftskip}{2em}
\setlength{\parskip}{5\lineskip}
\indent
"""

with open('phonR.md', 'a') as f:
	f.write(indent)
