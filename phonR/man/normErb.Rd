\name{normErb}
\title{Normalize formant values using the equivalent rectangular bandwidth scale}
\description{Transforms vowel formant data measured in Hertz to the ERB scale. 
			 Note that this returns the \dQuote{ERB-rate scale} value (i.e., the 
			 number of ERBs below the given frequency) rather than the ERB of 
			 the auditory filter centered at the given frequency.}
\usage{
    normErb(f)
}
\arguments{
    \item{f}{Vector or matrix of frequencies.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Glasberg, B. R., & Moore, B. C. J. 1990 \dQuote{Derivation of auditory filter shapes from notched-noise data.} \emph{Hearing Research}, 47(1-2), 103-138. \url{http://dx.doi.org/10.1016/0378-5955(90)90170-T}
}
\seealso{\code{\link{normVowels}}, \code{\link{normBark}}, 
		 \code{\link{normLobanov}}, \code{\link{normLog}}, 
		 \code{\link{normLogmean}}, \code{\link{normMel}}, 
		 \code{\link{normNearey}}, \code{\link{normWattFabricius}}}
\examples{
    data(indoVowelData)
	erbn <- with(indo, normErb(cbind(f1, f2)))
}
\keyword{methods}
