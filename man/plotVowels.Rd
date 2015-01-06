\name{plotVowels}
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
               ellipse.fill=FALSE, ellipse.conf=0.3173,
               ellipse.args=NULL, diph.arrows=FALSE,
               diph.args.tokens=NULL, diph.args.means=NULL,
               diph.label.first.only=TRUE,
               diph.mean.timept=1, diph.smooth=FALSE,
               force.heatmap=FALSE, force.colmap=NULL,
               force.res=50, force.method='default',
               force.legend=NULL, force.labels=NULL,
               force.label.pos=c(1, 3), var.col.by=NA,
               var.style.by=NA,  fill.opacity=0.3,
               legend.kwd=NULL, label.las=NULL,
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
                                            to \code{\link{polygon}}. Useful for
                                            controlling line width, etc. See
                                            Details for notes about color
                                            handling.}
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
    \item{ellipse.conf}{Size of the ellipse (0,1] expressed as an alpha-level
                        (i.e., 0.05 gives a 95\% confidence ellipse).  Defaults
                        to alpha level of 0.3173 (an ellipse encompassing
                        68.27\% of the data points, equivalent to plus-or-minus
                        1 sample standard deviation). Note that because this is
                        an alpha level, \emph{higher} numbers yield
                        \emph{smaller} ellipses.}
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
    \item{force.heatmap}{Logical; should a repulsive force heatmap be drawn?}
    \item{force.colmap}{Colormap to use when drawing the heatmap (see
                        \code{\link[plotrix]{color.scale}}. Note that the
                        heatmap may use semi-transparent colors, but this
                        transparency does not usually translate to the
                        \code{force.legend} colorbar, due to the way that
                        \code{\link[plotrix]{color.scale.lines}} works. In
                        short: the legend colorbar is made of lots of little
                        segments that don't quite touch when plotted, leaving
                        thin gaps in the colorbar where the background shows
                        through. In phonR this has been avoided by adding a
                        square cap to the line ends of each segment in the
                        colorbar, causing neighboring segments to overlap. This
                        overlapping is not noticeable when the
                        \code{force.colormap} uses fully opaque colors, but
                        usually yields a colorbar that looks opaque even when
                        the \code{force.colormap} colors are semi-transparent.
                        For better results, generate the \code{force.colmap}
                        with pale, opaque colors instead of intense colors that
                        are semi-transparent.}
    \item{force.res}{Resolution to use when drawing the heatmap. Warning: high
                     values of \code{force.res} can take a long time to
                     calculate and plot. See
                     \code{\link{repulsiveForceHeatmap}} for details.}
    \item{force.method}{String; method to use when assigning colors to the
                        heatmap. Options are \dQuote{default} and
                        \dQuote{pineda}; \dQuote{pineda} is considered
                        experimental and not currently supported. See
                        \code{\link{repulsiveForceHeatmap}} for details.}
    \item{force.legend.pos}{Location for plotting a color bar showing the range of
                            force heatmap values. Possible values are: a vector
                            \code{c(x1, x2, y1, y2)} defining the endpoints of a
                            line; \code{NA} to suppress plotting a color bar; and
                            \code{NULL} (the default) to plot it along the left side
                            of the graph, spanning from the bottom edge to halfway
                            up the left side of the plotting area.}
    \item{force.legend.labels}{Vector of strings (length 2); the labels to write at
                               each end of the force legend color bar. Ignored if
                               \code{force.heatmap} is \code{FALSE} or
                               \code{force.legend.pos} is \code{NA}.}
    \item{force.label.pos}{Vector of integers (length 2); position codes for the
                           colorbar labels. See the \code{pos} argument of
                           \code{\link[graphics]{text}} for explanation.}
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
         arguments are passed to \code{ellipse.args}, \code{poly.args}, or
         \code{hull.args}, then color is handled as follows: if
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
                      var.style.by=gender, pretty=TRUE))
    # simulate some diphthongs
indo_d <- within(indo, {
        f1b <- f1 + sample(c(-30:-5,5:30), length(f1), replace=TRUE)
        f2b <- f2 + sample(c(-50:-10,10:50), length(f2), replace=TRUE)
        })
    with(indo_d, plotVowels(f1=cbind(f1, f1b), f2=cbind(f2, f2b), vowel,
                        group=subj, diph.arrows=TRUE, pretty=TRUE))
}
\keyword{device}
\keyword{hplot}
