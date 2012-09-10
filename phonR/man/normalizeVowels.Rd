\name{normalizeVowels}
\alias{normalizeVowels}
\title{Normalize formant values using a variety of algorithms}
\description{Transforms vowel formant data measured in Hertz using one of several normalization schemes commonly used in phonetic and sociolinguistic research. Returns an n-by-m matrix of n data points by m formants in ascending order, with fundamental frequency first (if present), except for method \dQuote{wattfabricius} which only and always returns F1 and F2, regardless of whether f0 and F3 were supplied.}
\usage{
normalizeVowels(method, f0=NULL, f1=NULL, f2=NULL, f3=NULL,
vowel=NULL, grouping.factor=NULL)
}
\arguments{
    \item{method}{Normalization method to use.  Possible values are \dQuote{none}, \dQuote{bark}, \dQuote{mel}, \dQuote{erb}, \dQuote{log}, \dQuote{z}|\dQuote{zscore}|\dQuote{ztransform}|\dQuote{lobanov}, \dQuote{logmean}|\dQuote{nearey1}, \dQuote{nearey2}, \dQuote{s}|\dQuote{scentroid}|\dQuote{wattfabricius}.}
    \item{f0}{Vector of f0 (fundamental frequency) values. Required for method \dQuote{nearey2}.}
    \item{f1}{Vector of F1 (first formant) values. Required for methods \dQuote{nearey2}, \dQuote{wattfabricius}.}
    \item{f2}{Vector of F2 (second formant) values. Required for methods \dQuote{nearey2}, \dQuote{wattfabricius}.}
    \item{f3}{Vector of F3 (third formant) values. Required for method \dQuote{nearey2}.}
    \item{vowel}{Vector of vowel symbols. Required for method \dQuote{wattfabricius}, ignored for all other methods.}
    \item{grouping.factor}{Vector of grouping factor values. If supplied, used to calculate group-intrinsic normalizations for methods \dQuote{lobanov}, \dQuote{nearey1}, \dQuote{nearey2}, and \dQuote{wattfabricius}.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Glasberg, B. R., & Moore, B. C. J. 1990 \dQuote{Derivation of auditory filter shapes from notched-noise data.} \emph{Hearing Research}, 47(1-2), 103-138. \url{http://dx.doi.org/10.1016/0378-5955(90)90170-T}

Lobanov, B. M. 1971 \dQuote{Classification of Russian vowels spoken by different speakers.} \emph{The Journal of the Acoustical Society of America}, 49(2), 606-608. \url{http://dx.doi.org/10.1121/1.1912396}

McCloy, D. R. 2012 \dQuote{Normalizing and plotting vowels with the phonR package.} \emph{Technical Reports of the UW Linguistic Phonetics Laboratory}. \url{http://depts.washington.edu/phonlab/pubs/McCloy2012_phonR.pdf}

Nearey, T. M. 1978 \dQuote{Phonetic feature systems for vowels} (Doctoral dissertation, University of Alberta). Reprinted by the Indiana University Linguistics Club. \url{http://www.ualberta.ca/~tnearey/Nearey1978_compressed.pdf}

Stevens, S. S., & Volkmann, J. 1940 \dQuote{The relation of pitch to frequency: A revised scale.} \emph{The American Journal of Psychology}, 53(3), pp. 329-353.

Traunmuller, H. 1990 \dQuote{Analytical expressions for the tonotopic sensory scale.} \emph{The Journal of the Acoustical Society of America}, 88(1), 97-100. \url{http://dx.doi.org/10.1121/1.399849}

Watt, D., & Fabricius, A. H. 2002 \dQuote{Evaluation of a technique for improving the mapping of multiple speakers' vowel spaces in the F1 ~ F2 plane.} \emph{Leeds Working Papers in Linguistics and Phonetics}, 9, 159-173.

Zwicker, E., & Terhardt, E. 1980 \dQuote{Analytical expressions for critical-band rate and critical bandwidth as a function of frequency.} \emph{The Journal of the Acoustical Society of America}, 68(5), 1523-1525. \url{http://dx.doi.org/10.1121/1.385079}
}
\seealso{\code{\link{plotVowels}}}
\examples{
	data(indoVowelData)
	bark <- normalizeVowels('bark', f1=indo$f1, f2=indo$f2)
	wattfab <- normalizeVowels('wattfabricius', f1=indo$f1, f2=indo$f2, 
	           vowel=indo$vowel, grouping.factor=indo$subj)
}
\keyword{methods}
