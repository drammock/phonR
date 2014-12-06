\name{fill.triangle}
\title{Calculate a z-value for an arbitrary point (x, y) inside a triangle,
       based on the z-values of the triangle's vertices.}
\description{Uses Juan Pineda's filling algorithm, designed for continuous color
			 blending in 2-dimensions.}
\usage{
    fill.triangle(x, y, vertices)
}
    \item{x}{Numeric vector of x-coordinates.}
    \item{y}{Numeric vector of y-coordinates.}
    \item{vertices}{3-by-3 matrix of (x, y, z) values (columns) for the three 
                    vertices of the triangle (rows).}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Pineda, J. 1988 \dQuote{A parallel algorithm for polygon rasterization}. \emph{ACM SIGGRAPH Computer Graphics}, 22(4), 17–20. \url{http://dx.doi.org/10.1145/378456.378457}
}
\seealso{\code{\link{repulsive.force.heatmap}}}
\examples{
	require(plotrix)
	require(splancs)
    colmap <- color.scale(x=0:100, cs1=c(0, 180), cs2=100, cs3=c(25, 100), 
    	                  alpha=1, color.spec='hcl')
	vertices <- matrix(c(1, 4, 2, 1, 3, 4, length(colmap), 1, 30), nrow=3)
	xseq <- yseq <- seq(0, 5, 0.2)
	grid <- expand.grid(x=xseq, y=yseq)
    grid$z <- NA
    grid.indices <- inpip(grid, vertices[,1:2], bound=FALSE)
    grid$z[grid.indices] <- with(grid[grid.indices,], fill.triangle(x, y, vertices))
    par(mfrow=c(1,2))
    # points example
    with(grid, plot(x, y, col=colmap[round(z)], pch=16))
	segments(vertices[,1], vertices[,2], vertices[c(2,3,1),1], 
		     vertices[c(2,3,1),2])
	points(vertices[,1:2], pch=21, bg=colmap[vertices[,3]], cex=2)
	# image example
	image(xseq, yseq, matrix(grid$z, nrow=length(xseq)), col=colmap)
	segments(vertices[,1], vertices[,2], vertices[c(2,3,1),1], 
		     vertices[c(2,3,1),2])
	points(vertices[,1:2], pch=21, bg=colmap[vertices[,3]], cex=2)
}
\keyword{methods}
