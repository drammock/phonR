\name{normLog}
\title{Normalize formant values using a simple log transform (base 10).}
\description{Transforms vowel formant data measured in Hertz log-Hertz.}
\usage{
    normLog(f)
}
\arguments{
    \item{f}{Vector or matrix of frequencies.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{}
\seealso{\code{\link{normVowels}}, \code{\link{normBark}}, 
		 \code{\link{normErb}}, \code{\link{normLobanov}}, 
		 \code{\link{normLogmean}}, \code{\link{normMel}}, 
		 \code{\link{normNearey}}, \code{\link{normWattFabricius}}}
\examples{
    data(indoVowelData)
	lognormed <- with(indo, normLog(cbind(f1, f2)))
}
\keyword{methods}
