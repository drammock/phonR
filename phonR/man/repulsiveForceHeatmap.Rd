\name{repulsiveForceHeatmap}
\title{Plot a heatmap of repulsive force values for a vowel space.}
\description{}
\usage{
    repulsiveForceHeatmap(x, y, type=NULL, resolution=50, colormap=NULL, 
    					  method='default', add=TRUE, ...)
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
    \item{colormap}{Colormap to use when drawing the heatmap (see 
                    \code{\link{plotrix::color.scale}}. See 
                    \code{\link{forceHeatmapLegend}} for notes regarding the
                    use of semi-transparent colors in heatmaps; in brief, it
                    works fine for the heatmap itself, but works poorly in the
                    legend colorbar, so avoiding semi-transparent colors is
                    recommended if plotting a legend colorbar (otherwise the
                    legend will not match the heatmap, impeding interpretation.}
    \item{method}{String; method to use when assigning colors to the heatmap. 
                  The value of \dQuote{default} assigns each grid point a vowel identity based on its nearest neighbor and assigns a color 
                  value based on the repulsive force that would exist for a 
                  vowel at that location (if it had been in the dataset). The 
                  only other accepted value is \dQuote{pineda}, which uses Juan 
                  Pineda's triangle filling algorithm; this is considered 
                  experimental and not supported.}
    \item{...}{Additional arguments passed to \code{\link{graphics::image}},
               the most important of which is probably \code{add}.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Liljencrants, J., & Lindblom, B. 1972 \dQuote{Numerical simulation of vowel quality systems: The role of perceptual contrast}. \emph{Language}, 48(4), 839–862. \url{http://www.jstor.org/stable/411991}

McCloy, D. R., Wright, R. A., & Souza, P. E. 2014 \dQuote{Talker versus dialect effects on speech intelligibility: A symmetrical study}. \emph{Language and Speech}. \url{http://dx.doi.org/10.1177/0023830914559234}

Pineda, J. 1988 \dQuote{A parallel algorithm for polygon rasterization}. \emph{ACM SIGGRAPH Computer Graphics}, 22(4), 17–20. \url{http://dx.doi.org/10.1145/378456.378457}
}
\seealso{\code{\link{plot.vowels}}, \code{\link{repulsive.force}}}
\examples{
    require(plotrix)
    data(indoVowelData)
    colmap <- color.scale(x=0:100, cs1=c(0, 180), cs2=100, cs3=c(25, 100), 
    	                    alpha=0.5, color.spec='hcl')
    with(indo[indo$subj==indo$subj[1],], 
    	 repulsiveForceHeatmap(f2, f1, type=vowel, resolution=10, 
    	 					   colormap=colmap, add=FALSE))
}
\keyword{methods}
