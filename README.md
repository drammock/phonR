phonR
=============

This is a package (under development) for the R language built with phoneticians and phonologists in mind. Currently it is just a couple of functions for normalizing and plotting vowels, but the plots are rather beautiful and customizable. Currently only handles a single F1/F2 value per vowel, but will be expanded to handle higher-dimensional representations of vowels soon (i.e., diphthongs and/or duration data).

The biggest barrier to entry to using the plotting function is its reliance on the R library "Cairo", which in turn depends on the Cairo graphics backend.  Installing the Cairo backend is easy under Linux, slightly less so for Mac, and somewhat troublesome on Windows.  To that end, an "installing Cairo" tutorial is also included.

Planned future functionality is undecided, but will likely include some functions related to analysis of distinctive feature systems, since I already wrote some scripts related to that. If there's a particular functionality you'd like to see implemented, or want to contribute one, drop me a line.
