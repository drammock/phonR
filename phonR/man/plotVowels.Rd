\name{plotVowels}
\alias{plotVowels}
\title{Plot F1 and F2 values from separate vectors or from a data frame}
\description{Generates high-quality plots of provided formant values using either the default onscreen device (X11, Quartz, or Win32) or direct-to-file using R base graphics functions (PDF, SVG, JPG, PNG, TIFF, or BMP). Can also normalize values on-the-fly before plotting, through a call to \code{normalizeVowels}.}
\usage{
	plotVowels(data=NULL, vowel=NULL, f1=NULL, f2=NULL, f3=NULL, f0=NULL, 
	grouping.factor=NULL, norm.method='none', match.unit=TRUE, 
	match.axes='absolute', points='text', means='text', points.alpha=0.5, 
	means.alpha=1, points.cex=0.6, means.cex=1.2, ignore.hidden=TRUE, 
	ellipses=TRUE, ellipse.alpha=0.3173, polygon=TRUE, poly.order=NULL, 
	poly.include=NULL, single.plot=TRUE, titles='auto', axis.titles='auto', 
	axis.cex=0.8, garnish.col='#666666FF', grayscale=FALSE, 
	vary.colors=!grayscale, vary.shapes=grayscale, vary.lines=grayscale, 
	legend=single.plot, output='screen', family='', pointsize=12, 
	units='in', width=6.5, height=6.5, res=72, asp=NULL)
}
\arguments{
    \item{data}{Optional data frame containing the values to be plotted.  If \code{data} is specified, the values of \code{f0}, \code{f1}, \code{f2}, \code{f3}, \code{vowel}, and \code{grouping.factor} should be enclosed in quotes and should reference column names in \code{data}.}
    \item{vowel}{Vector (or column name in \code{data}) of vowel symbols. If \code{data} is specified and \code{vowel} is NULL, it looks for a column in \code{data} called \dQuote{vowel}. If \code{data} and \code{vowel} are both NULL, it looks for an object named vowel.}
    \item{f1}{Vector (or column name in \code{data}) of F1 values. If \code{data} is specified and \code{f1} is NULL, it looks for a column in \code{data} called \dQuote{f1}. If \code{data} and \code{f1} are both NULL, it looks for an object named f1.}
    \item{f2}{Vector (or column name in \code{data}) of F2 values. If \code{data} is specified and \code{f2} is NULL, it looks for a column in \code{data} called \dQuote{f2}. If \code{data} and \code{f2} are both NULL, it looks for an object named f2.}
    \item{f3}{Vector (or column name in \code{data}) of F3 values (required for normalization method \dQuote{nearey2}).}
    \item{f0}{Vector (or column name in \code{data}) of f0 values (required for normalization method \dQuote{nearey2}).}
    \item{grouping.factor}{Vector (or column from \code{data}) of grouping factor values. Allows for plotting data by color/shape/linestyle (if \code{single.plot} is TRUE), or separating to individual plots (if \code{single.plot} is FALSE).}
    \item{norm.method}{Normalization method to use to transform data before plotting.  Possible values are \dQuote{none}, \dQuote{bark}, \dQuote{mel}, \dQuote{erb}, \dQuote{log}, \dQuote{z}|\dQuote{zscore}|\dQuote{ztransform}|\dQuote{lobanov}, \dQuote{logmean}|\dQuote{nearey1}, \dQuote{nearey2}, \dQuote{s}|\dQuote{scentroid}|\dQuote{wattfabricius}.}
    \item{match.unit}{If TRUE (and \code{norm.method} is not \dQuote{none}), axis tickmarks will be in the normalized unit. If FALSE (and \code{norm.method} is not \dQuote{none}), axis tickmark values will be in Hertz, with tickmark locations scaled according to the chosen normalization method.}
    \item{match.axes}{If \dQuote{absolute}, all plots have same bounds. If \dQuote{relative}, all plots span the same range, but may have different endpoints. If \dQuote{none}, extrema are calculated separately for each plot.  Ignored (coerced to \dQuote{absolute}) if \code{single.plot} is TRUE.}
    \item{points}{If \dQuote{text}, plots vowel tokens with the character string given in \code{vowel}. If \dQuote{shape}, plots vowel tokens as geometric shapes.  If \dQuote{none}, omits plotting the vowel tokens.}
    \item{means}{If \dQuote{text}, plots vowel means with the character string given in \code{vowel}. If \dQuote{shape}, plots vowel means as geometric shapes.  If \dQuote{none}, omits plotting the vowel means.}
    \item{points.alpha}{Opacity of individual vowel points (permissible values between 0 and 1 inclusive.)}
    \item{means.alpha}{Opacity of vowel means (permissible values between 0 and 1 inclusive.)}
    \item{points.cex}{Size of individual vowel points relative to \code{pointsize}.}
    \item{means.cex}{Size of vowel means relative to \code{pointsize}.}
    \item{ignore.hidden}{If TRUE and if \code{points} is \dQuote{none}, plotting dimensions are calculated based only on means, (or ellipses, if present).}
    \item{ellipses}{If TRUE, plot an ellipse around each vowel mean tracing an equidensity contour of the bivariate normal distribution.}
    \item{ellipse.alpha}{Size of the ellipse [0,1] expressed as an alpha-level (i.e., 0.05 gives a 95\% confidence ellipse).  Defaults to alpha level of 0.3173 (an ellipse encompassing 68.27\% of the data points, equivalent to plus-or-minus 1 standard deviation).  Note that because this is an alpha level, HIGHER numbers give SMALLER ellipses.}
    \item{polygon}{If TRUE, draw a series of line segments connecting the vowel means, in the order given by \code{poly.order}.}
    \item{poly.order}{Vector of strings determining the order in which connecting lines are drawn for the vowel polygon. Should match the levels of the factor \code{vowel}. If there are values of \code{vowel} not included in \code{poly.order}, the function will do its best to recognize them and put them in a sensible order but polygon drawing results may be ugly.}
    \item{poly.include}{Indicates how many of the vowels in \code{poly.order} should be connected into a polygon.  To exclude vowels from the polygon drawing, put them at the end of \code{poly.order} and provide a value for \code{poly.include} shorter than the length of \code{poly.order}.}
    \item{single.plot}{If TRUE, plot each value of the grouping factor on the same graph.  If FALSE and \code{output} is \dQuote{screen}, result is an on-screen lattice plot.  If FALSE and \code{output} is \dQuote{pdf} or \dQuote{jpg}, result is a collection of separate PDF or JPG files (one per level of \code{grouping.factor}).}
    \item{titles}{If \dQuote{auto}, will auto-generate titles based on \code{grouping.factor}.  If \dQuote{none}, titles are omitted.  Also accepts a single string (all titles will be the same), or a vector of strings that matches the number of levels in \code{grouping.factor}.}
    \item{axis.titles}{If \dQuote{auto}, will auto-generate axis labels based on \code{norm.method}.  Also accepts a two-element vector of strings (useful for cases where the F1 and F2 values passed to the function have already been normalized).  Order of elements is F2 (horizontal) label before F1 (vertical) label.}
	\item{axis.cex}{Size of the axis numbers relative to \code{pointsize}.}
    \item{garnish.col}{Color for the axis lines, ticks, numbers, and labels.}
    \item{grayscale}{If TRUE, plot without color.  Note that if \code{grayscale} is FALSE, graphs may still come out colorless if \code{uniform.style} is TRUE and there is only one group on the graph, because the first color plotted defaults to black.}
    \item{vary.colors}{If TRUE, vary token and mean colors by group.  Defaults to the opposite value as \code{grayscale}.  If both \code{grayscale} and \code{vary.colors} are TRUE, colors are shades of gray varying in equal luminance steps.}
    \item{vary.shapes}{If TRUE, vary token and mean shapes by group (when either \code{points} or \code{means} has the value \dQuote{shape}).  Defaults to same value as \code{grayscale}.  Ignored if \code{uniform.style} is TRUE.}
    \item{vary.lines}{If TRUE, vary line style by group (when \code{ellipses} or \code{polygon} are TRUE).  Defaults to same value as \code{grayscale}.  Ignored if \code{uniform.style} is TRUE.}
    \item{legend}{If TRUE, prints a legend in the lower left corner of the graph. Also accepts the standard keyword values for the base graphics \code{legend()} command (\dQuote{top}, \dQuote{topright}, \dQuote{right}, \dQuote{bottomright}, \dQuote{bottom}, \dQuote{bottomleft}, \dQuote{left}, \dQuote{topleft}, \dQuote{center}).}
    \item{output}{Possible values are \dQuote{screen}, \dQuote{pdf}, \dQuote{svg}, \dQuote{jpg}, \dQuote{tif}, \dQuote{png}, \dQuote{bmp}.}
    \item{family}{Character string indicating the name of the font family to use for all text in the plot. If \code{points='text'} or \code{means='text'} and \code{vowels} includes non-ASCII unicode IPA symbols, be sure to specify a font that has glyphs at those codepoints. Note that font specification may fail if plotting to screen and then saving as PDF or Postscript from the onscreen plot window menu. To ensure PDF font fidelity, run plotVowels() with \code{output='pdf'} instead.}
    \item{pointsize}{Base size of font used for plotting.}
    \item{units}{Unit of plot dimensions: \dQuote{in}, \dQuote{cm}, \dQuote{mm}, or \dQuote{px}.  Ignored when \code{output} is \dQuote{screen}.}
    \item{width}{Width of the plot.  Ignored when \code{output} is \dQuote{screen}.}
    \item{height}{Height of the plot.  Ignored when \code{output} is \dQuote{screen}.}
    \item{res}{Resolution of output in dots-per-inch. Ignored for screen or vector output formats (pdf, svg), defaults to 72dpi for raster formats.}
    \item{asp}{Aspect ratio for plot area. Default is NULL, which changes the aspect ratio to maximize use of the available plot area; if specified, must be a positive numeric value.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
McCloy, D. R. 2012 \dQuote{Normalizing and plotting vowels with the phonR package.} \emph{Technical Reports of the UW Linguistic Phonetics Laboratory}. \url{http://depts.washington.edu/phonlab/pubs/McCloy2012_phonR.pdf}
}
\seealso{\code{\link{normalizeVowels}}}
\examples{
	data(indoVowelData)
	plotVowels(vowel=indo$vowel, f1=indo$f1, f2=indo$f2, grouping.factor=
	           indo$gender, norm.method='bark', match.unit=FALSE, 
			   poly.order=c('i','e','a','o','u'))
	plotVowels(data=indo, grouping.factor='subj', single.plot=FALSE, 
	           match.axes='relative', points='none', means.cex=1.5,
			   poly.order=c('i','e','a','o','u'))
}
\keyword{device}
\keyword{hplot}
