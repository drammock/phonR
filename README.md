phonR
=============

This is a package for the R language built with phoneticians and phonologists in mind. Currently it is just a couple of functions for normalizing and plotting vowels, but the plots are rather beautiful and customizable. Supports plotting both monophthongs and diphthongs; future versions may incorporate higher-dimensional representations of vowels (i.e., adding F3 or duration).

The "installing Cairo" tutorial is a remnant from earlier versions of phonR that relied on the Cairo R library, which in turn relied on the Cairo graphics backend.  Since version 0.3-1, phonR has used R base graphics and the Cairo R library is no longer required; the instructions are preserved for posterity, in case someone wants to get Cairo working for other reasons.

Planned future functionality includes a heatmap-type pixel plot that lays behind the vowel plot, a function for calculating polygonal area (via a few different methods), and possibly some functions related to analysis of distinctive feature systems, since I already have some scripts related to that. Also planned is a function for plotting a vowel space with arrows and lines to indicate harmony classes, and a function for generating standard consonant tables.  Long-term goals include porting Sean Fulop's code for reassigned spectrograms from MATLAB into R. If there's a particular functionality you'd like to see implemented, or want to contribute one, drop me a line.
