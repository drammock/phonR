\name{repulsiveForce}
\alias{repulsiveForce}
\alias{repulsiveForceHeatmap}
\alias{forceHeatmapLegend}
\title{Repulsive force calculations and plots for vowel systems.}
\description{For each point, calculates the sum of inverse squared distances to 
             all points that are not of the same type.}
\usage{
    repulsiveForce(x, y, type)
    repulsiveForceHeatmap(x, y, type=NULL, resolution=50, colormap=NULL, 
                          method='default', ...)
    forceHeatmapLegend(x, y, smoothness=50, colormap=NULL, lend=2, lwd=6, ...)
}
\arguments{
    \item{x}{Numeric vector of x values (e.g., F2 frequencies).}
    \item{y}{Numeric vector of y values (e.g., F1 frequencies).}
    \item{type}{Attribute of the \code{(x, y)} points used to interpolate values
                for intermediate points. In typical linguistic usage of this
                function, \code{type} would be the vowel identities of the F2
                and F1 values passed to \code{x} and \code{y} (respectively).}
    \item{resolution}{Number of points to interpolate between each axis unit.
                      Higher resolution yields smoother heatmaps at the cost of
                      increased computational time. NOTE: in typical linguistic
                      usage, an appropriate \code{resolution} value will depend
                      on the type of units used to plot the formant data (e.g.,
                      you will need higher resolution for vowels plotted on the
                      Bark scale to get an equivalently smooth heatmap to one
                      plotted with lower resolution on a Hertz scale.)}
    \item{smoothness}{Number of color steps to use when drawing the legend 
    				  colorbar.}
    \item{colormap}{Colormap to use when drawing the heatmap and legend (see 
                    \code{\link[plotrix]{color.scale}}. Note that although the 
                    heatmap may use semi-transparent colors, this transparency 
                    does not usually translate well to the \code{force.legend} 
                    colorbar, due to the way that 
                    \code{\link[plotrix]{color.scale.lines}} works. In short: 
                    the legend colorbar is made of lots of little segments that 
                    don't quite touch when plotted, leaving thin gaps in the 
                    colorbar where the background shows through. In phonR this 
                    has been avoided by adding a square cap to the line ends of 
                    each segment in the colorbar, causing neighboring segments 
                    to overlap. This overlapping is not noticeable when the 
                    \code{force.colormap} uses fully opaque colors, but usually 
                    yields a colorbar that looks opaque even when the 
                    \code{force.colormap} colors are semi-transparent. For better 
                    results, generate the \code{force.colmap} with pale, opaque 
                    colors instead of intense colors that are semi-transparent.}
    \item{method}{String; method to use when assigning colors to the heatmap. 
                  The value of \dQuote{default} assigns each grid point a vowel 
                  identity based on its nearest neighbor and assigns a color 
                  value based on the repulsive force that would exist for a 
                  vowel at that location (if it had been in the dataset). The 
                  only other accepted value is \dQuote{pineda}, which uses Juan 
                  Pineda's triangle filling algorithm (which is faster, but may
                  appear discontinuous at the edges of adjacent triangles).}
    \item{lend}{End-cap style for the individual segments of the colorbar. See 
                \code{\link[graphics]{par}}.}
    \item{lwd}{Width of the colorbar. See \code{\link[graphics]{par}}.}
    \item{...}{Additional arguments passed by \code{forceHeatmapLegend} to 
               \code{\link[plotrix]{color.scale.lines}}, or by 
               \code{repulsiveForceHeatmap} to \code{\link[graphics]{image}},
               (the most important of which is probably \code{add}).}
}
\details{Given endpoints \code{x} and \code{y}, \code{forceHeatmapLegend} draws 
         a colorbar legend with \code{smoothness} number of steps using the 
         provided colormap (or defaults to grayscale if \code{colormap} is 
         \code{NULL}.}
\value{\code{repulsiveForce} returns the sum of the repulsive forces calculated
       at each point \code{(x,y)}.}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Liljencrants, J., & Lindblom, B. 1972 \dQuote{Numerical simulation of vowel quality systems: The role of perceptual contrast}. \emph{Language}, 48(4), 839-862. \url{http://www.jstor.org/stable/411991}

McCloy, D. R., Wright, R. A., & Souza, P. E. 2014 \dQuote{Talker versus dialect effects on speech intelligibility: A symmetrical study}. \emph{Language and Speech}. \url{http://dx.doi.org/10.1177/0023830914559234}

Pineda, J. 1988 \dQuote{A parallel algorithm for polygon rasterization}. \emph{ACM SIGGRAPH Computer Graphics}, 22(4), 17-20. \url{http://dx.doi.org/10.1145/378456.378457}
}
\seealso{\code{\link{plotVowels}}}
\examples{
    require(plotrix)
    data(indoVowels)
    force <- with(indo[indo$subj==indo$subj[1],], repulsiveForce(f2, f1, vowel))
    colmap <- color.scale(x=0:100, cs1=c(0, 180), cs2=100, cs3=c(25, 100), 
                          alpha=0.5, color.spec='hcl')
    with(indo[indo$subj==indo$subj[1],], 
        repulsiveForceHeatmap(f2, f1, type=vowel, resolution=10, 
                              colormap=colmap, add=FALSE))
    xl <- rep(max(range(indo$f2)), 2)
    yl <- rev(range(indo$f1)) + c(0, abs(diff(range(indo$f1)) / 2))
    forceHeatmapLegend(xl, yl, colormap=colmap)
}
\keyword{methods}
