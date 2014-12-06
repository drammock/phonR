\name{norm.logmean}
\title{Normalize formant values using the logmean transform.}
\description{Transforms vowel formant data measured in Hertz using the logmean
             transform (the log of the frequency minus the mean of the logs of
             all formant frequencies for that formant).}
\usage{
    norm.logmean(f, group=NULL, ...)
}
\arguments{
    \item{f}{Vector or matrix of frequencies. If a matrix, each column should be
    		 a single frequency type (f0, F1, F2, etc.), and the rows should be
    		 individual measurements.}
    \item{group}{Vector or factor indicating groups of elements of \code{f} (if 
    			 \code{f} is a vector), or rows of \code{f} (if \code{f} is a
    			 matrix) that should be normalized together. This is useful for,
    			 e.g., normalizing different talkers individually with a single
    			 function call.}
    \item{...}{Additional arguments passed to \code{\link{base::colMeans}} (e.g., 
    		   \code{na.rm=TRUE}).}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Nearey, T. M. 1978 \dQuote{Phonetic feature systems for vowels} (Doctoral dissertation, University of Alberta). Reprinted by the Indiana University Linguistics Club. \url{http://www.ualberta.ca/~tnearey/Nearey1978_compressed.pdf}
}
\seealso{\code{\link{norm.vowels}}, \code{\link{norm.bark}}, 
		 \code{\link{norm.erb}}, \code{\link{norm.lobanov}}, 
		 \code{\link{norm.log}}, \code{\link{norm.mel}}, 
		 \code{\link{norm.nearey}}, \code{\link{norm.wattfabricius}}}
\examples{
    data(indoVowelData)
	logmean <- with(indo, norm.logmean(cbind(f1, f2), group=gender)
}
\keyword{methods}
