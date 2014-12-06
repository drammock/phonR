\name{convex.hull.area}
\title{Calculate convex hull area}
\description{Calculate the area of the convex hull surrounding all points.}
\usage{
    convex.hull.area(x, y, group=NULL)
}
\arguments{
    \item{x}{Vector of x-values.}
    \item{y}{Vector of y-values.}
    \item{group}{Vector or factor indicating groupings of points to fit separate 
                 convex hulls to. If \code{NULL} (the default), a single hull 
                 will be generated for all points.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{}
\seealso{\code{\link{grDevices::chull}}, \code{\link{splancs::areapl}}}
\examples{
    data(indoVowelData)
    with(indo, convex.hull.area(f2, f1))
    with(indo, convex.hull.area(f2, f1, gender))
}
\keyword{}
