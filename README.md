# phonR
#### tools for phoneticians and phonologists

Just want to learn to use `phonR`? Read the [tutorial vignette](http://drammock.github.io/phonR).

This is a package for the R language built for phoneticians and phonologists. Its main claim-to-fame is the `plotVowels` function, which supports all the standard R output formats (screen, pdf, eps, svg, tiff, png, jpg), and can plot vowel polygons, ellipses, convex hulls, and heatmaps. `phonR` also allows plotting data points with IPA symbols using custom fonts, and distinguishing speaker groups by color, symbol, and/or linestyle. It also supports plotting diphthongs (or any arbitrary number of measurement timepoints, really), with optional arrowheads for indicating formant movement direction.
 
The second main offering of `phonR` is a set of functions for normalization of formant frequency data. Currently there are 8 normalization methods implemented: bark, ERB, Lobanov, log, mel, logmean (“Nearey1”), shared logmean (“Nearey2”), and Watt-Fabricius. A third capability of `phonR` worth highlighting is the implementation of repulsive force calculations, and the ability to plot heatmaps to represent repulsive force across the vowel space. Helper functions are also included for calculating the area of a vowel space polygon or convex hull.
