\name{normNearey}
\title{Normalize formant values using the \dQuote{Nearey 2} method.}
\description{Transforms vowel formant data measured in Hertz using Nearey's
			transformation.}
\usage{
    normNearey(f, group=NULL, ...)
}
\arguments{
    \item{f}{N-by-4 matrix of frequencies, with columns \dQuote{f0}, \dQuote{F1}, 
    		 \dQuote{F2}, \dQuote{F3}.}
    \item{group}{Vector or factor indicating rows of \code{f} that should be 
    			 normalized together. This is useful for, e.g., normalizing 
    			 different talkers individually with a single function call.}
    \item{...}{Additional arguments passed to \code{\link{base::colMeans}} (e.g., 
    		   \code{na.rm=TRUE}).}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Nearey, T. M. 1978 \dQuote{Phonetic feature systems for vowels} (Doctoral dissertation, University of Alberta). Reprinted by the Indiana University Linguistics Club. \url{http://www.ualberta.ca/~tnearey/Nearey1978_compressed.pdf}
}
\seealso{\code{\link{normVowels}}, \code{\link{normBark}}, 
		 \code{\link{normErb}}, \code{\link{normLobanov}}, 
		 \code{\link{normLog}}, \code{\link{normLogmean}}, 
		 \code{\link{normMel}}, \code{\link{normWattFabricius}}}
\examples{}
\keyword{methods}
