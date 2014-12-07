\name{vowelMeansPolygonArea}
\title{Calculate the area of a polygon connecting vowel formant means.}
\description{The polygon's vertices are the means of the vowel formant 
			 frequencies given by \code{f1} and \code{f2}, and are connected in
			 the order specified in \code{poly.order}.}
\usage{
    vowelMeansPolygonArea(f1, f2, vowel, poly.order)
}
\arguments{
    \item{f1}{Numeric vector of first formant frequencies.}
    \item{f2}{Numeric vector or second formant frequencies.}
    \item{vowel}{Vector or factor of vowel identifiers (typically a character 
    			 vector, though numeric will work).}
    \item{poly.order}{Order in which the polygon vertices should be connected.
    				  Should contain each value in \code{vowel} once.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{}
\seealso{\code{\link{plotVowels}}}
\examples{
    data(indoVowelData)
	area <- with(indo, vowelMeansPolygonArea(f1, f2, vowel, 
				 poly.order=c('i', 'e', 'a', 'o', 'u')))
}
\keyword{methods}
