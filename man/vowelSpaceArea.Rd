\name{Vowel space area functions}
\alias{convexHullArea}
\alias{vowelMeansPolygonArea}
\title{Calculate the area of a vowel space}
\description{Calculate the area of an \code{F2 x F1} vowel space, either as the area of a polygon connecting vowel formant means, or the area of a convex hull encompassing all tokens.}
\usage{
    convexHullArea(f1, f2, group=NULL)
    vowelMeansPolygonArea(f1, f2, vowel, poly.order, group=NULL)
}
\arguments{
    \item{f1}{Numeric vector of first formant frequencies.}
    \item{f2}{Numeric vector or second formant frequencies.}
    \item{vowel}{Vector or factor of vowel identifiers (typically a character 
                 vector, though numeric will work).}
    \item{poly.order}{Order in which the polygon vertices should be connected.
                      Should contain each value in \code{vowel} once.}
    \item{group}{Vector or factor indicating groupings of points to fit separate 
                 convex hulls to. If \code{NULL} (the default), a single hull 
                 will be generated for all points.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\seealso{\code{\link[grDevices]{chull}}, \code{\link[splancs]{areapl}}}
\examples{
    data(indoVowels)
    hull.area <- with(indo, convexHullArea(f1, f2, group=subj))
    poly.area <- with(indo, vowelMeansPolygonArea(f1, f2, vowel, 
        poly.order=c("i", "e", "a", "o", "u"), group=subj))
}
\keyword{methods}
