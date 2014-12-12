\name{makeTransparent}
\alias{makeTransparent}
\title{Change the value of the alpha channel of an RGB(A) color value to a given
       opacity value in the range [0, 1].}
\description{This is an internal helper function for \code{plotVowels}.}
\usage{
    makeTransparent(color, opacity)
}
\arguments{
    \item{color}{An RGB or RGBA color in hexadecimal format.}
    \item{opacity}{Number in the range [0, 1].}
}
\value{An RGBA color in hexadecimal format.}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\seealso{\code{\link{plotVowels}}}
\examples{
    opaque.color <- rgb(0.1, 0.2, 0.3)
    trans.color <- makeTransparent(opaque.color, 0.5)
}
