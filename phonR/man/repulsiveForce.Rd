\name{repulsiveForce}
\title{Calculate repulsive force of a vowel system}
\description{For each point, calculates the sum of inverse squared distances to all points that are not of the same type.}
\usage{
    repulsiveForce(x, y, type)
}
\arguments{
    \item{x}{Vector of x-values.}
    \item{y}{Vector of y-values.}
    \item{type}{Vector or factor indicating groupings of points.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{Liljencrants, J., & Lindblom, B. 1972. \dQuote{Numerical simulation of vowel quality systems: The role of perceptual contrast.} Language, 48(4), 839-862.}
\seealso{}
\examples{
    data(indoVowelData)
    force <- with(indo[indo$subj==indo$subj[1],], repulsiveForce(f2, f1, vowel))
}
\keyword{}
