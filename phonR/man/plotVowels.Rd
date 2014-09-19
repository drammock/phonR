\name{plotVowels}
\alias{plotVowels}
\title{Plot F1 and F2 values}
\description{Generates high-quality plots of provided formant values using either the default onscreen device (X11, Quartz, or Win32) or direct-to-file using built-in R file output methods (PDF, SVG, JPG, PNG, TIFF, or BMP).}
\usage{
    plotVowels(f1, f2, vowel=NULL, group=NULL, plot.tokens=TRUE,
    pch.tokens=NULL, cex.tokens=NULL, plot.means=FALSE, pch.means=NULL, 
    cex.means=NULL, ellipse.line=FALSE, ellipse.fill=FALSE, ellipse.conf=0.3173,
    hull.line=FALSE, hull.fill=FALSE, hull.col=NULL, poly.order=NA,
    poly.line=FALSE, poly.fill=FALSE, poly.col=NULL, force.heatmap=FALSE,
    force.colmap=NULL, force.res=50, force.method='default', col.by=NA, 
    style.by=NA, axis.labels=NULL, pretty=FALSE, diphthong.smooth=FALSE, 
    diphthong.arrows=FALSE, output='screen', units=NULL, color.palette=NULL,
    legend=NULL, ...)
}
\arguments{
    \item{f1}{Vector or matrix of F1 values. To plot multiple timepoints for each vowel, \code{f1} should be a matrix with vowel tokens varying along the rows and timepoints varying across the columns.}
    \item{f2}{Vector or matrix of F2 values. To plot multiple timepoints for each vowel, \code{f1} should be an N-by-M matrix with vowel tokens varying along the rows and timepoints varying across the columns.}
    \item{vowel}{Vector of vowel symbols/labels.}
    \item{group}{Vector or factor that determines the groups used in calculating vowel means, e.g., a factor indicating \dQuote{gender}, \dQuote{speaker}, \dQuote{sociolinguistic register}, etc.}
    \item{plot.tokens}{Logical; should individual vowel tokens be plotted?}
    \item{pch.tokens}{Vector of strings or integers; the symbol(s) to use when plotting vowel tokens.}
    \item{cex.tokens}{Numeric; size of individual vowel points relative to \code{par("cex")}.}
    \item{plot.means}{Logical; should individual vowel tokens be plotted?}
    \item{pch.means}{Vector of strings or integers; the symbol(s) to use when plotting vowel means.}
    \item{cex.means}{Size of vowel means relative to \code{par("cex")}.}
    \item{ellipse.line}{Logical; should vowel density ellipses be drawn with an outer line?}
    \item{ellipse.fill}{Logical; should vowel density ellipses be filled?}
    \item{ellipse.conf}{Size of the ellipse (0,1] expressed as an alpha-level (i.e., 0.05 gives a 95\% confidence ellipse).  Defaults to alpha level of 0.3173 (an ellipse encompassing 68.27\% of the data points, equivalent to plus-or-minus 1 sample standard deviation). Note that because this is an alpha level, HIGHER numbers give SMALLER ellipses.}
    \item{hull.line}{Logical; should the convex hull encompassing all tokens be drawn with an outer line?}
    \item{hull.fill}{Logical; should the convex hull encompassing all tokens be filled?}
    \item{hull.col}{Vector of colors to use when drawing the convex hulls. If \code{NULL}, defaults to the values in \code{palette()} at 40\% alpha, or if \code{pretty==TRUE}, to equally-spaced HCL values at 30\% alpha.}
    \item{poly.order}{Vector of strings determining the order in which connecting lines are drawn for the vowel means polygon. Values should match the levels of \code{factor(vowel)}.  If there are values of \code{vowel} not included in \code{poly.order}, they will not be connected to the polygon line.}
    \item{poly.line}{Logical; should the vowel means polygon be drawn with an outer line?}
    \item{poly.fill}{Logical; should the vowel means polygon be filled?}
    \item{poly.col}{Vector of colors to use when drawing the vowel means polygons. If \code{NULL}, defaults to the values in \code{color.palette} at the opacity set by \code{fill.opacity}.}
    \item{force.heatmap}{Logical; should a repulsive force heatmap be drawn?}
    \item{force.colmap}{Colormap to use when drawing the heatmap (see \code{\link{plotrix::color.scale()}}. Note that the heatmap may use semi-transparent colors, but this transparency does not usually translate to the \code{force.legend} colorbar, due to the way that \code{\link{plotrix::color.scale.lines()}} works. In short: the legend colorbar is made of lots of little segments that don't quite touch when plotted, leaving thin gaps in the colorbar where the background shows through. In phonR this has been avoided by adding a square cap to the line ends of each segment in the colorbar, causing neighboring segments to overlap. This overlapping is not noticeable when the \code{force.colormap} uses fully opaque colors, but usually yields a colorbar that looks opaque even when the \code{force.colormap} colors are semi-transparent. For better results, generate the \code{force.colmap} with pale, opaque colors instead of intense colors that are semi-transparent.}
    \item{force.res}{Resolution to use when drawing the heatmap. Warning: high values of \code{force.res} can take a long time to calculate and plot. See \code{\link{force.heatmap}} for details.}
    \item{force.method}{String; method to use when assigning colors to the heatmap. The value of \dQuote{default} assigns each grid point a vowel identity based on its nearest neighbor and assigns a color value based on the repulsive force that would exist for that vowel at that location (if it had been in the dataset). The only other accepted value is \dQuote{pineda}, which uses Juan Pineda's triangle filling algorithm; this is considered experimental and not supported.}
    \item{force.legend}{Location for plotting a color bar showing the range of force heatmap values. Possible values are: a vector \code(c(x1, x2, y1, y2)) defining the endpoints of a line; \code{NA} to suppress plotting a color bar; and \code{NULL} (the default) to plot it along the left edge of the graph, from the bottom to halfway up.}
    \item{force.labels}{Vector of strings (length 2); the labels to write at each end of the force legend color bar. Ignored if \code{force.heatmap} is \code{FALSE} or \code{force.legend} is \code{NA}.}
    \item{force.label.pos}{Vector of integers (length 2); position codes for the colorbar labels. See the \code{pos} argument of \code{\link{graphics::text()}} for explanation.}
    \item{col.by}{Vector or factor indicating the dimension along which to vary color.}
    \item{style.by}{Vector or factor indicating the dimension along which to vary linetype and plotting symbol.}
    \item{axis.labels}{Two-element character vector of axis labels. If \code{NULL}, will default to \code{c("F2", "F1")}}
    \item{pretty}{Logical; a switch that sets various graphical parameters: \code{mar}, \code{las}, \code{mgp}, \code{ann}, \code{xpd}, \code{fg}, and \code{tcl}, as well as arrow parameters \code{length} and \code{angle} and \code{plot.vowels}-specific parameters such as \code{color.palette}. It is permissible to set \code{pretty=TRUE} and also pass any of these parameters to \code{plot.vowels} to override the \code{pretty} defaults.}
    \item{diphthong.smooth}{Logical; should a smoothing spline be drawn for each vowel instead of segments connecting individual timepoints?}
    \item{diphthong.arrows}{Logical; should the last timepoint of each vowel be marked with an arrowhead?}
    \item{output}{Graphical device to plot to. Supported values are \dQuote{screen}, \dQuote{pdf}, \dQuote{svg}, \dQuote{jpg}, \dQuote{tif}, \dQuote{png}, \dQuote{bmp}.}
    \item{units}{Unit of plot dimensions: \dQuote{in}, \dQuote{cm}, \dQuote{mm}, or \dQuote{px}.  Ignored when \code{output} is \dQuote{screen}.}
    \item{color.palette}{Color palette to use for plotting. If \code{NULL}, defaults to \code{palette()} (unless \code{pretty==TRUE}, in which case defaults to equally spaced HCL hues).}
    \item{fill.opacity}{Numeric in the range [0,1], indicating the opacity of color fills for ellipses, vowel means polygons, and convex hulls (if drawn).}
    \item{...}{Other graphical parameters passed to methods; e.g., \code{width}, \code{height}, \code{asp}, \code{res}, etc.}
    \item{legend.kwd}{Keyword indicating legend placement (see \code{\link{legend}}). If \code{NULL} (the default), no legend will be printed.}
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
			   poly.order=c('i','e','a','o','u'), grayscale=TRUE)
	plotVowels(data=indo, grouping.factor='subj', single.plot=FALSE, 
	           match.axes='relative', points='none', means.cex=1.5,
			   poly.order=c('i','e','a','o','u'))
	# simulate some diphthongs
	indo$f1b <- indo$f1+sample(c(-30:-5,5:30), length(indo$f1), replace=TRUE)
	indo$f2b <- indo$f2+sample(c(-30:-5,5:30), length(indo$f1), replace=TRUE)
	plotVowels(vowel=indo$vowel, f1=cbind(indo$f1,indo$f1b), 
	           f2=cbind(indo$f2,indo$f2b), grouping.factor=indo$subj, 
	           ellipses=FALSE, polygon=FALSE)
}
\keyword{device}
\keyword{hplot}
