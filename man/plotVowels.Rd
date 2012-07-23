\name {plotVowels}
\alias{plotVowels}
\title{Plot F1 and F2 values from separate vectors or from a data frame}
\description{Generates high-quality plots of provided formant values using either the default onscreen device (X11, Quartz, or Win32) or direct-to-file using the Cairo graphics package (PDF or JPG).}
\usage{
plotVowels(data=myDataFrame, f1='F1ColumnName', f2='F2ColumnName', vowel='vowelColumnName', ...)

plotVowels(f1=F1ValuesVector, f2=F2ValuesVector, vowel=VowelCodesVector, ...)
}
\arguments{
	\item{data}{Optional data frame containing the values to be plotted.  If \code{data} is specified, be sure to enclose the values of \code{f0}, \code{f1}, \code{f2}, \code{f3}, \code{vowel}, and \code{grouping.factor} in quotes.}
	\item{vowel}{Vector (or column from \code{data}) of vowel symbols.}
	\item{f1}{Vector (or column from \code{data}) of F1 values.}
	\item{f2}{Vector (or column from \code{data}) of F2 values.}
	\item{f3}{Vector (or column from \code{data}) of F3 values (needed for normalization method \dQuote{nearey2}).}
	\item{f0}{Vector (or column from \code{data}) of f0 values (needed for normalization method \dQuote{nearey2}).}
	\item{grouping.factor}{Vector (or column from \code{data}) of grouping factor values. Allows for plotting data by color/shape/linestyle (if \code{single.plot} is TRUE), or separating to individual plots (if \code{single.plot} is FALSE).}
	\item{norm.method}{Normalization method to use to transform data before plotting.  Possible values are \dQuote{none}, \dQuote{bark}, \dQuote{mel}, \dQuote{erb}, \dQuote{log}, \dQuote{z}|\dQuote{zscore}|\dQuote{ztransform}|\dQuote{lobanov}, \dQuote{logmean}|\dQuote{nearey1}, \dQuote{nearey2}, \dQuote{s}|\dQuote{scentroid}|\dQuote{wattfabricius}.}
	\item{match.unit}{If TRUE (and \code{norm.method} is not \dQuote{none}), axis tickmarks will be in the normalized unit. If FALSE (and \code{norm.method} is not \dQuote{none}), axis tickmark values will be in Hertz, with tickmark locations scaled according to the chosen normalization method.}
	\item{match.axes}{If \dQuote{absolute}, all plots have same bounds. If \dQuote{relative}, all plots span the same range, but may have different endpoints. If \dQuote{none}, extrema are calculated separately for each plot.  Ignored (coerced to \dQuote{absolute}) if \code{single.plot} is TRUE.}
	\item{points}{If \dQuote{text}, plots vowel tokens with the character string given in \code{vowel}. If \dQuote{shape}, plots vowel tokens as geometric shapes.  If \dQuote{none}, omits plotting the vowel tokens.}
	\item{means}{If \dQuote{text}, plots vowel means with the character string given in \code{vowel}. If \dQuote{shape}, plots vowel means as geometric shapes.  If \dQuote{none}, omits plotting the vowel means.}
	\item{points.alpha}{Opacity of individual vowel points (permissible values between 0 and 1 inclusive.)}
	\item{means.alpha}{Opacity of vowel means (permissible values between 0 and 1 inclusive.)}
	\item{ignore.hidden}{If TRUE and if \code{points} is \dQuote{none}, plotting extrema are calculated based only on means, (or ellipses, if present).}
	\item{ellipses}{If TRUE, plot an ellipse around each vowel mean tracing an equidensity contour of the bivariate normal distribution.}
	\item{ellipse.size}{Size of the ellipse [0,1] expressed as an alpha-level (i.e., 0.05 gives a 95% confidence ellipse).  Defaults to alpha level of 0.3173 (equivalent to an ellipse encompassing plus-or-minus 1 standard deviation from the mean).  Note that because this is an alpha level, HIGHER numbers give SMALLER ellipses.}
	\item{polygon}{If TRUE, plot a line connecting the vowel means in the order given by \code{poly.order}.}
	\item{poly.order}{Vector of strings determining the order in which connecting lines are drawn for the vowel polygon. Should match the levels of the factor \code{vowel}.}
	\item{poly.include}{Indicates how many of the vowels in \code{poly.order} should be connected into a polygon.  To exclude vowels from the polygon drawing, put them at the end of \code{poly.order} and provide a value for \code{poly.include} shorter than the length of \code{poly.order}.}
	\item{single.plot}{If TRUE, plot each value of the grouping factor on the same graph.  If FALSE and \code{output} is \dQuote{screen}, result is an on-screen lattice plot.  If FALSE and \code{output} is \dQuote{pdf} or \dQuote{jpg}, result is a collection of separate PDF or JPG files (one per level of \code{grouping.factor}).}
	\item{axis.col}{Color for the axis lines, ticks, numbers, and labels.}
	\item{titles}{If \dQuote{none}, titles are omitted.  If \dQuote{auto}, will auto-generate titles based on \code{grouping.factor}.  Also accepts a single string (all titles will be the same), or a vector of strings that matches the number of levels in \code{grouping.factor}.}
	\item{grayscale}{If TRUE, plot without color.  Note that if \code{grayscale} is FALSE, graphs may still come out colorless if \code{uniform.style} is TRUE and there is only one group on the graph, because the first color plotted defaults to black.}
	\item{vary.shapes}{If TRUE, vary token or mean shapes by group (when either \code{points} or \code{means} has the value \dQuote{shape}).  Defaults to same value as \code{grayscale}.  Ignored if \code{uniform.style} is TRUE.}
	\item{vary.lines}{If TRUE, vary line style by group (when \code{ellipses} or \code{polygon} are TRUE).  Defaults to same value as \code{grayscale}.  Ignored if \code{uniform.style} is TRUE.}
	\item{uniform.style}{If TRUE, plot each group's data using the same color, shape, and linestyle.  If unspecified, defaults to the opposite value as \code{single.plot}.}
	\item{legend}{If TRUE, prints a legend in the lower left corner of the graph.}
	\item{aspect.ratio}{Aspect ratio for plot area. Default is NULL, which changes the aspect ratio to maximize use of the available plot area; if specified, must be a positive numeric value.}
	\item{plot.dims}{A vector of length two, giving width & height of the plot.  Ignored when \code{output} is \dQuote{screen}.}
	\item{plot.unit}{Unit of plot dimensions: \dQuote{in}, \dQuote{cm}, etc.  Ignored when \code{output} is \dQuote{screen}.}
	\item{output}{Possible values are \dQuote{screen}, \dQuote{pdf}, \dQuote{jpg}.}
}
% \details{}
% \value{}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
% \references{McCloy, D. R. 2012 \dQuote{The phonR package for vowel plotting.} \emph{Working Papers of the UW Linguistic Phonetics Laboratory}. \url{}}
\seealso{\code{\link{normalizeVowels}}}
% \examples{}
\keyword{device}
\keyword{hplot}
