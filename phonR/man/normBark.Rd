\name{normBark}
\title{Normalize formant values using the Bark transform.}
\description{Transforms vowel formant data measured in Hertz to the Bark scale.}
\usage{
    normBark(f)
}
\arguments{
    \item{f}{Vector or matrix of frequencies.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Traunmuller, H. 1990 \dQuote{Analytical expressions for the tonotopic sensory scale.} \emph{The Journal of the Acoustical Society of America}, 88(1), 97-100. \url{http://dx.doi.org/10.1121/1.399849}
}
\seealso{\code{\link{normVowels}}, \code{\link{normErb}},
		 \code{\link{normLobanov}}, \code{\link{normLog}}, 
		 \code{\link{normLogmean}}, \code{\link{normMel}}, 
		 \code{\link{normNearey}}, \code{\link{normWattFabricius}}}
\examples{
    data(indoVowelData)
	bark <- with(indo, normBark(cbind(f1, f2)))
}
\keyword{methods}
