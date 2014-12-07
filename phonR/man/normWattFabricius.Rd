\name{normWattFabricius}
\title{Normalize formant values using the s-centroid method.}
\description{Transforms vowel formant data measured in Hertz using the 
			 s-centroid transform. The implementation varies slightly from the 
			 formula in Watt & Fabricius (2002), since \code{normWattFabricius} 
			 simply calculates which vowel has the highest mean F1 value and 
			 designates it as low corner of the triangle, rather than asking the 
			 user to expressly specify the \dQuote{TRAP} or \dQuote{START} vowel. 
			 Similarly, \code{normWattFabricius} simply calculates which vowel 
			 has the highest mean F2 value and uses that to calculate the upper 
			 left corner, rather than expressly looking for the mean of the 
			 \dQuote{point-vowel} /i/. The upper right corner is, as in the 
			 original method, derived from the other two.}
\usage{
    normWattFabricius(f, vowel, group=NULL)
}
\arguments{
    \item{f}{N-by-2 matrix or data frame of F1 and F2 values.}
    \item{vowel}{Vector or factor of vowel symbols, with 
                 \code{length(vowel)==nrow(f)}.}
    \item{group}{Vector or factor indicating rows of \code{f} that should be 
    			 normalized together. This is useful for, e.g., normalizing 
    			 different talkers individually with a single function call.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Watt, D., & Fabricius, A. H. 2002 \dQuote{Evaluation of a technique for improving the mapping of multiple speakers' vowel spaces in the F1 ~ F2 plane.} \emph{Leeds Working Papers in Linguistics and Phonetics}, 9, 159-173.
}
\seealso{\code{\link{normVowels}}, \code{\link{normBark}}, 
		 \code{\link{normErb}}, \code{\link{normLobanov}}, 
		 \code{\link{normLog}}, \code{\link{normLogmean}}, 
		 \code{\link{normMel}}, \code{\link{normNearey}}}
\examples{
    data(indoVowelData)
	wattfab <- with(indo, normWattFabricius(cbind(f1, f2), vowel, group=subj))
}
\keyword{methods}
