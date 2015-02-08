\name{Plot vowel formant frequencies}
\alias{plotVowels}
\title{Plot vowel formant data and a variety of derivative measures.}
\description{Generates high-quality plots of provided formant values using
             either the default onscreen device (X11, Quartz, or Win32) or
             direct-to-file using built-in R file output methods (PDF, SVG, JPG,
             PNG, TIFF, or BMP).}
\usage{
    plotVowels(f1, f2, vowel=NULL, group=NULL,
               plot.tokens=TRUE, pch.tokens=NULL,
               cex.tokens=NULL, alpha.tokens=NULL,
               plot.means=FALSE, pch.means=NULL,
               cex.means=NULL, alpha.means=NULL,
               hull.line=FALSE, hull.fill=FALSE,
               hull.args=NULL, poly.line=FALSE,
               poly.fill=FALSE, poly.args=NULL,
               poly.order=NA, ellipse.line=FALSE,
               ellipse.fill=FALSE, ellipse.conf=0.6827,
               ellipse.args=NULL, diph.arrows=FALSE,
               diph.args.tokens=NULL, diph.args.means=NULL,
               diph.label.first.only=TRUE,
               diph.mean.timept=1, diph.smooth=FALSE,
               heatmap=FALSE, heatmap.args=NULL,
               heatmap.legend=FALSE, heatmap.legend.args=NULL,
               var.col.by=NULL, var.style.by=NULL,
               fill.opacity=0.3, label.las=NULL,
               legend.kwd=NULL, legend.args=NULL,
               pretty=FALSE, output='screen', ...)
}
\arguments{
    \item{f1,f2}{Vector or matrix of formant frequency values. To plot multiple
                 timepoints for each vowel, \code{f1} and \code{f2} should be
                 matrices with vowel tokens varying along the rows and
                 timepoints varying across the columns.}
    \item{vowel}{Vector of vowel symbols/labels.}
    \item{group}{Vector or factor that determines the groups used in calculating
                 vowel means, e.g., a factor indicating \dQuote{gender},
                 \dQuote{speaker}, \dQuote{sociolinguistic register}, etc.}
    \item{plot.tokens}{Logical; should individual vowel tokens be plotted?}
    \item{pch.tokens}{Vector of strings or integers; the symbol(s) to use when
                      plotting vowel tokens. Integers are interpreted as
                      standard R \code{pch} values (see
                      \code{\link[graphics]{points}}).}
    \item{cex.tokens}{Numeric; size of individual vowel points relative to
                      \code{par("cex")}.}
    \item{alpha.tokens}{Numeric in range [0,1], indicating opacity of plotted
                        vowel tokens.}
    \item{plot.means}{Logical; should individual vowel tokens be plotted?}
    \item{pch.means}{Vector of strings or integers; the symbol(s) to use when
                     plotting vowel means. Integers are interpreted as standard
                     R \code{pch} values (see \code{\link[graphics]{points}}).}
    \item{cex.means}{Size of vowel means relative to \code{par("cex")}.}
    \item{alpha.means}{Numeric in range [0,1], indicating opacity of plotted
                       vowel means.}
    \item{hull.line}{Logical; should a line be drawn tracing the convex hull
                     encompassing all tokens (separately for each level of
                     \code{group})?}
    \item{hull.fill}{Logical; should the convex hull(s) have a color fill?}
    \item{hull.args,poly.args,ellipse.args}{Named list of arguments to be passed
                                            to \code{\link[graphics]{polygon}}.
                                            Useful for controlling line width,
                                            etc. See "Details" for notes about
                                            color handling.}
    \item{poly.line}{Logical; should a line be drawn tracing the polygon
                     connecting the mean values for each vowel (separately for
                     each level of \code{group})?}
    \item{poly.fill}{Logical; should the polygon(s) connecting mean values for
                     each vowel have a color fill?}
    \item{poly.order}{Vector or factor indicating the order in which the polygon
                      vertices should be connected. Should match the levels of
                      \code{factor(vowel)}. If there are values of \code{vowel}
                      not included in \code{poly.order}, they will not be
                      connected to the polygon line.}
    \item{ellipse.line}{Logical; should vowel density ellipses be drawn with an
                        outer line?}
    \item{ellipse.fill}{Logical; should vowel density ellipses be filled?}
    \item{ellipse.conf}{Size of the ellipse (0,1] expressed as a confidence level
                        (i.e., 0.95 gives a 95\% confidence ellipse).  Defaults
                        to a confidence level of 0.6827 (equivalent to
                        plus-or-minus 1 sample standard deviation).}
    \item{diph.arrows}{Logical; should the last timepoint of each vowel be
                       marked with an arrowhead?}
    \item{diph.args.tokens,diph.args.means}{List of named arguments to be passed
        to \code{\link[graphics]{points}} and/or \code{\link[graphics]{arrows}}
        when plotting tokens or means (ignored if \code{f1} and \code{f2} are
        1-dimensional). Default is to plot bare lines (\code{type="l"}) when
        \code{pch.tokens} is \code{NULL}, to draw points and lines overplotted
        (\code{type="o"}) when \code{diph.arrows=FALSE}, and to plot bare
        lines with the first timepoint of each diphthong plotted when
        \code{diph.arrows=TRUE}. When \code{pretty=TRUE} and
        \code{diph.arrows=TRUE}, additional default settings are
        \code{length=0.1} and \code{angle=20}. For \code{diph.args.means},
        \code{lwd} defaults to \code{2 * par("lwd")} when \code{pretty=TRUE}.
        All of these defaults are overridden by values passed to
        \code{diph.args.tokens} or \code{diph.args.means} in the function
        call.}
    \item{diph.label.first.only}{Logical; if plotting diphthongs, should a symbol
                                 or label be drawn only at the first timepoint?
                                 Note that if plotting means, the label or symbol
                                 may not correspond to the \emph{first} timepoint;
                                 it depends on the value of \code{diph.mean.timept}.}
    \item{diph.mean.timept}{A strictly positive integer indicating which timepoint
                            of the diphthongs should be used to calculate means,
                            ellipses, and polygons. For example, if the \code{f1}
                            and \code{f2} arguments are N-by-5 matrices where the
                            3rd column of each represents the formant measurement
                            at the vowel midpoint (with the other columns providing
                            2 onglide and 2 offglide measurements), you can plot the
                            means, ellipses, and polygons based on the midpoint
                            measures by specifying \code{diph.mean.timept=3}.}
    \item{diph.smooth}{Logical; should a smoothing spline be drawn instead of
                       segments connecting individual timepoints? This feature
                       is under development, unstable, unsupported, and to the
                       extent that it is implemented, it is only implemented for
                       tokens, not means.}
    \item{heatmap}{Logical; should a repulsive force heatmap be drawn?}
    \item{heatmap.args}{Named list of additional arguments passed to
                        \code{\link{repulsiveForceHeatmap}}. The arguments \code{x},
                        \code{y}, and \code{type} are passed automatically by
                        \code{plotVowels}; \code{heatmap.args} is probably most
                        useful for controlling \code{resolution}, \code{colormap},
                        and the \code{xform} function.}
    \item{heatmap.legend}{Logical; should a legend be drawn showing the color scale
                          used in the repulsive force heatmap?}
    \item{heatmap.legend.args}{Named list of additional arguments passed to
                               \code{\link{repulsiveForceHeatmapLegend}}. Parameters
                               likely to be user-specified here are \code{x} and \code{y}
                               for specifying the endpoints of the colorbar, \code{labels}
                               for label text at the two ends of the colorbar, and
                               \code{smoothness} for the number of color steps to display
                               (limited by the number of levels in the color scale
                               used). See \code{\link{repulsiveForceHeatmapLegend}} for
                               a more complete description of the available arguments.}
    \item{var.col.by}{Vector or factor indicating the dimension along which to vary
                      color.}
    \item{var.style.by}{Vector or factor indicating the dimension along which to
                        vary linetype and plotting symbol.}
    \item{fill.opacity}{Number in the range [0, 1] indicating the opacity of
                        color fills for ellipses, hulls, and polygons (if drawn).
                        Does not affect \code{force.heatmap} colors, which are
                        specified via \code{force.colmap}.}
    \item{legend.kwd}{Keyword indicating legend placement (see
                      \code{\link[graphics]{legend}}). If \code{NULL} (the
                      default), no legend will be printed.}
    \item{label.las}{Controls the orientation of axis labels relative to the axis
                     line (independently from the axis tick numbers). Possible
                     values are integers 0-4 (see \code{\link[graphics]{par}}),
                     or \code{NULL} (to use the default value of \code{par("las")},
                     or a value passed to \code{plotVowels} as \code{las} if
                     available).}
    \item{legend.args}{Named list of additional arguments to be passed to
                       \code{\link[graphics]{legend}}, for controlling things
                       like \code{inset}, \code{ncol}, \code{seg.len}, etc.}
    \item{pretty}{Logical; a switch that sets various graphical parameters:
                  \code{mar}, \code{las}, \code{mgp}, \code{xpd}, \code{fg}, and
                  \code{tcl}, as well as arrow parameters like \code{length} and
                  \code{angle} and \code{plotVowels}-specific parameters such
                  as \code{color.palette}. It is permissible to set
                  \code{pretty=TRUE} and also pass any of these parameters to
                  \code{plotVowels} to override the \code{pretty} defaults.}
    \item{output}{Graphical device to plot to. Supported values are
                  \dQuote{screen}, \dQuote{pdf}, \dQuote{svg}, \dQuote{jpg},
                  \dQuote{tif}, \dQuote{png}, \dQuote{bmp}.}
    \item{...}{Other graphical parameters passed to methods; e.g., \code{width},
               \code{height}, \code{units}, \code{asp}, \code{res}, \code{xlim},
               \code{xlab}, \code{main}, etc. Two arguments of \code{plot}:
               \code{ann} and \code{type}, are always silently overridden by
               \code{plotVowels}.}
}
\details{\emph{Notes on color handling}. If no \code{col} or \code{border}
         arguments are passed to \code{hull.args}, \code{poly.args}, or
         \code{ellipse.args}, then color is handled as follows: if
         \code{pretty=FALSE}, colors default to the values in \code{palette()},
         with opacity for fills set by \code{fill.opacity}. If \code{pretty=TRUE},
         equally-spaced hues of HCL colors are used instead of \code{palette()}.
         If the values passed to \code{var.col.by} and \code{vowel} are identical,
         hull and polygon lines are drawn in black, and fills are drawn black
         with appropriate \code{fill.opacity}.
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\seealso{\code{\link{normVowels}}}
\examples{
data(indoVowels)
with(indo, plotVowels(f1, f2, vowel, group=gender, plot.means=TRUE,
                      pch.means=vowel, ellipse.line=TRUE, poly.line=TRUE,
                      poly.order=c('i','e','a','o','u'), var.col.by=vowel,
                      var.style.by=gender, pretty=TRUE, alpha.tokens=0.3,
                      cex.means=2))
# simulate some diphthongs
f1delta <- sample(c(-10:-5, 5:15), nrow(indo), replace=TRUE)
f2delta <- sample(c(-15:-10, 20:30), nrow(indo), replace=TRUE)
f1coefs <- matrix(sample(c(2:5), nrow(indo) * 2, replace=TRUE), 
                  nrow=nrow(indo))
f2coefs <- matrix(sample(c(3:6), nrow(indo) * 2, replace=TRUE),
                  nrow=nrow(indo))
indo <- within(indo, {
    f1a <- f1  + f1delta * f1coefs[,1]
    f2a <- f2  + f2delta * f2coefs[,1]
    f1b <- f1a + f1delta * f1coefs[,2]
    f2b <- f2a + f2delta * f2coefs[,2]
    })
with(indo, plotVowels(cbind(f1, f1a, f1b), cbind(f2, f2a, f2b), vowel,
                      group=gender, plot.tokens=TRUE, pch.tokens=NA,
                      alpha.tokens=0.3, plot.means=TRUE, pch.means=vowel,
                      var.col.by=vowel, var.style.by=gender, pretty=TRUE,
                      diph.arrows=TRUE, diph.args.tokens=list(lwd=0.8),
                      diph.args.means=list(lwd=2)))
}
\keyword{device}
\keyword{hplot}
