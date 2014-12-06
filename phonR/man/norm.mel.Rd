\name{norm.mel}
\title{Normalize formant values using the Mel scale.}
\description{Transforms vowel formant data measured in Hertz to the Mel scale
             (a perceptual scale that attempts to represent equal pitch intervals
             with equal mel differences).}
\usage{
    norm.mel(f)
}
\arguments{
    \item{f}{Vector or matrix of frequencies.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Stevens, S. S., & Volkmann, J. 1940 \dQuote{The relation of pitch to frequency: A revised scale.} \emph{The American Journal of Psychology}, 53(3), pp. 329-353.
}
\seealso{\code{\link{norm.vowels}}, \code{\link{norm.bark}}, 
		 \code{\link{norm.erb}}, \code{\link{norm.lobanov}}, 
		 \code{\link{norm.log}}, \code{\link{norm.logmean}}, 
		 \code{\link{norm.nearey}}, \code{\link{norm.wattfabricius}}}
\examples{
    data(indoVowelData)
	erbn <- with(indo, norm.erb(cbind(f1, f2))
}
\keyword{methods}
