\name{norm.lobanov}
\title{Normalize formant values using Lobanov's method.}
\description{Transforms vowel formant data measured in Hertz using Lobanov's 
             method (a standard z-transform, typically within-speaker).}
\usage{
    norm.lobanov(f, group=NULL)
}
\arguments{
    \item{f}{Vector or matrix of frequencies.}
    \item{group}{Vector or factor indicating groups of elements of \code{f} (if 
    			 \code{f} is a vector), or rows of \code{f} (if \code{f} is a
    			 matrix) that should be normalized together. This is useful for,
    			 e.g., normalizing different talkers individually with a single
    			 function call.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Lobanov, B. M. 1971 \dQuote{Classification of Russian vowels spoken by different speakers.} \emph{The Journal of the Acoustical Society of America}, 49(2), 606-608. \url{http://dx.doi.org/10.1121/1.1912396}
}
\seealso{\code{\link{norm.vowels}}, \code{\link{norm.bark}}, 
		 \code{\link{norm.erb}}, \code{\link{norm.log}}, 
		 \code{\link{norm.logmean}}, \code{\link{norm.mel}}, 
		 \code{\link{norm.nearey}}, \code{\link{norm.wattfabricius}}}
\examples{
    data(indoVowelData)
	lobanov <- with(indo, norm.lobanov(cbind(f1, f2), group=subj)
}
\keyword{methods}
