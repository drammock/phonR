\name{normMel}
\title{Normalize formant values using the Mel scale.}
\description{Transforms vowel formant data measured in Hertz to the Mel scale
             (a perceptual scale that attempts to represent equal pitch intervals
             with equal mel differences).}
\usage{
    normMel(f)
}
\arguments{
    \item{f}{Vector or matrix of frequencies.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Stevens, S. S., & Volkmann, J. 1940 \dQuote{The relation of pitch to frequency: A revised scale.} \emph{The American Journal of Psychology}, 53(3), pp. 329-353.
}
\seealso{\code{\link{normVowels}}, \code{\link{normBark}}, 
		 \code{\link{normErb}}, \code{\link{normLobanov}}, 
		 \code{\link{normLog}}, \code{\link{normLogmean}}, 
		 \code{\link{normNearey}}, \code{\link{normWattFabricius}}}
\examples{
    data(indoVowelData)
	mel <- with(indo, normMel(cbind(f1, f2)))
}
\keyword{methods}
