\name{norm.log}
\title{Normalize formant values using a simple log transform (base 10).}
\description{Transforms vowel formant data measured in Hertz log-Hertz.}
\usage{
    norm.log(f)
}
\arguments{
    \item{f}{Vector or matrix of frequencies.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{}
\seealso{\code{\link{norm.vowels}}, \code{\link{norm.bark}}, 
		 \code{\link{norm.erb}}, \code{\link{norm.lobanov}}, 
		 \code{\link{norm.logmean}}, \code{\link{norm.mel}}, 
		 \code{\link{norm.nearey}}, \code{\link{norm.wattfabricius}}}
\examples{
    data(indoVowelData)
	lognormed <- with(indo, norm.log(cbind(f1, f2))
}
\keyword{methods}
