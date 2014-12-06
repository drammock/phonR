phonR
=============

This is a package for the R language built with phoneticians and phonologists in mind. Its main claim-to-fame is the `plot.vowels` function, which supports all the standard output formats (screen, pdf, eps, svg, tiff, png, jpg), vowel polygons, ellipses, convex hulls, heatmaps, custom fonts, and distinguishing speaker groups by color, symbol, and/or linestyle. It also supports plotting diphthongs (or any arbitrary number of measurement timepoints, really), with optional arrowheads for indicating formant movement direction.

The second main purpose of `phonR` is for normalization of formant frequency data. Currently there are 8 normalization methods implemented: bark, ERB, Lobanov, log, logmean, mel, Nearey, and Watt-Fabricius. The third aspect of `phonR` worth highlighting is the implementation of repulsive force calculations, and plotting heatmaps to represent repulsive force across the vowel space. Various helper functions are also included for things like calculating the area of a vowel space polygon, spacing axis tickmarks, drawing ellipses, making smooth color gradients in 2-dimensions, or changing the opacity of an RGBA color.

The "installing Cairo" tutorial is a remnant from earlier versions of phonR that relied on the Cairo R library, which in turn relied on the Cairo graphics backend.  Since version 0.3-1, phonR has used R base graphics and the Cairo R library is no longer required; the instructions are preserved for posterity, in case someone wants to get Cairo working for other reasons.
