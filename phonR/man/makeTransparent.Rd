\name{makeTransparent}
\title{Change the value of the alpha channel of an RGB(A) color value to a given
       opacity value in the range [0, 1].}
\description{This is an internal helper function for \code{plot.vowels}.}
\usage{
    makeTransparent(color, opacity)
}
\arguments{
    \item{color}{An RGB or RGBA color in hexadecimal format.}
    \item{opacity}{Number in the range [0, 1].}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{}
\seealso{\code{\link{plot.vowels}}}
\examples{
    opaque.color <- rgb(0.1, 0.2, 0.3)
    trans.color <- makeTransparent(opaque.color, 0.5)
}
\keyword{}
