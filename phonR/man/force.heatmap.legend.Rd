\name{force.heatmap.legend}
\title{Draw a colorbar legend for repulsive force heatmaps.}
\description{}
\usage{
    force.heatmap.legend(x, y, smoothness=50, colormap=NULL, lend=2, lwd=6, ...)
}
    \item{x}{Numeric vector of length 2, specifying the x-coordinates of the
             ends of the colorbar.}
    \item{y}{Numeric vector of length 2, specifying the y-coordinates of the
             ends of the colorbar.}
    \item{smoothness}{Number of color steps to use when drawing the legend 
    				  colorbar.}
    \item{colormap}{Colormap to use when drawing the heatmap legend (see 
                    \code{\link{plotrix::color.scale()}}. Note that although the 
                    heatmap may use semi-transparent colors, this transparency 
                    does not usually translate well to the \code{force.legend} 
                    colorbar, due to the way that 
                    \code{\link{plotrix::color.scale.lines()}} works. In short: 
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
    \item{lend}{End-cap style for the individual segments of the colorbar. See 
    			\code{\link{par}}.}
    \item{lwd}{Width of the colorbar. See \code{\link{par}}.}
    \item{...}{Additional arguments passed to 
    		   \code{\link{plotrix::color.scale.lines}}.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{}
\seealso{\code{\link{repulsive.force.heatmap}}, 
		 \code{\link{plot.vowels}}}
\examples{
	# TODO: add example
}
\keyword{}
