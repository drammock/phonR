\name{Normalize vowel formant frequencies}
\alias{normVowels}
\alias{normBark}
\alias{normErb}
\alias{normLobanov}
\alias{normLog}
\alias{normLogmean}
\alias{normMel}
\alias{normNearey1}
\alias{normNearey2}
\alias{normSharedLogmean}
\alias{normWattFabricius}
\title{Normalize formant frequency values using a variety of algorithms}
\description{Functions for transforming vowel formant frequency data measured in
             Hertz, using one of several normalization schemes commonly used in
             phonetic and sociolinguistic research. \code{normVowels} is a
             convenience function wrapping to the individual \code{norm[Method]}
             functions.}
\usage{
    normBark(f)
    normErb(f)
    normLog(f)
    normMel(f)
    normLobanov(f, group=NULL)
    normLogmean(f, group=NULL, exp=FALSE, ...)
    normNearey1(f, group=NULL, exp=FALSE, ...)
    normNearey2(f, group=NULL, exp=FALSE, ...)
    normSharedLogmean(f, group=NULL, exp=FALSE, ...)
    normWattFabricius(f, vowel, group=NULL)
    normVowels(method, f0=NULL, f1=NULL, f2=NULL, f3=NULL, 
               vowel=NULL, group=NULL, ...)
}
\arguments{
    \item{f}{Vector or matrix of formant frequencies. For \code{normNearey},
             \code{f} must be an N-by-4 matrix of frequencies, with column order
             \dQuote{f0}, \dQuote{F1}, \dQuote{F2}, \dQuote{F3}. For
             \code{normWattFabricius}, \code{f} must be an N-by-2 matrix or data
             frame of F1 and F2 values. If passing a matrix to
             \code{normLogmean}, formants must be grouped within columns, not
             rows.}
    \item{vowel}{Vector or factor of vowel symbols, with
                 \code{length(vowel)==nrow(f)}. Used only in
                 \code{normVowels(method="wattfabricius", ...)} or
                 \code{normWattFabricius(...)}.}
    \item{group}{Vector or factor indicating rows of \code{f} that should be
                 normalized together. This is useful for, e.g., calculating
                 talker-intrinsic normalizations when \code{group} encodes
                 talker identity.}
    \item{exp}{Logical; should the result of the logmeans calculation be passed
               through the \code{\link[base]{exp}} function before being returned?}
    \item{f0,f1,f2,f3}{Separate vectors of formant or
        fundamental frequency values used in the convenience method
        \code{plotVowels}. \code{f1} and \code{f2} are required when
        \code{method} is \code{"wattfabricius"}, \code{"logmean"},
        \code{"shared"}, \code{"nearey1"}, or \code{"nearey2"}.}
    \item{method}{Specification of the normalization method to use when calling
                  the convenience method \code{normVowels}. Possible values are
                  \dQuote{bark}, \dQuote{erb}, \dQuote{lobanov},
                  \dQuote{log}, \dQuote{logmean}, \dQuote{mel},
                  \dQuote{shared}, and \dQuote{wattfabricius}.
                  \dQuote{zscore} is an accepted synonym for \dQuote{lobanov};
                  \dQuote{nearey1} is an accepted synonym for \dQuote{logmean};
                  \dQuote{nearey2} is an accepted synonym for \dQuote{shared};
                  and \dQuote{scentroid}, is an accepted synonym for
                  \dQuote{wattfabricius}.}
    \item{...}{Additional arguments passed to \code{\link[base]{colMeans}} by
               functions \code{normLogmean} and \code{normSharedLogmean} (useful for
               specifying the value of \code{na.rm}).}
}
\details{
    \code{normLogmean} is a synonym for \code{normNearey1}, which is also sometimes
    confusingly called \dQuote{single logmean}. \code{normSharedLogmean} is a
    synonym for \code{normNearey2}. The argument \code{exp=TRUE} for these functions
    will yield values that are consistent with the \code{\link[vowels]{norm.nearey}}
    implementation, which takes the result of Nearey's original formulae and uses
    it as the exponent of the base of the natural logarithm (presumably so that
    the function always yields positive values).

    Note that \code{normErb} returns the \dQuote{ERB-rate scale} value (i.e.,
    the number of ERBs below the given frequency), not the ERB of the auditory
    filter centered at the given frequency.

    The implementation of the Watt-Fabricius method varies slightly from the
    formula in Watt & Fabricius (2002), since \code{normWattFabricius} simply
    calculates which vowel has the highest mean F1 value and designates it as
    the low corner of the triangle, rather than asking the user to expressly
    specify the \dQuote{TRAP} or \dQuote{START} vowel. Similarly,
    \code{normWattFabricius} simply calculates which vowel has the highest mean
    F2 value and uses that to calculate the upper left corner, rather than
    expressly looking for the mean of the \dQuote{point-vowel} /i/. The upper
    right corner is, as in the original method, derived from the other two. If
    the vowels with the highest mean F1 and highest mean F2 are not the same
    pair of vowels for all members of \code{group}, \code{normWattFabricius}
    returns an error.
}
\value{Most of the functions return a vector or matrix of the same dimensions as
       were passed in. The exceptions are \code{normVowels}, which returns an
       n-by-m matrix of n data points by m formants with formants in ascending
       order with fundamental frequency first (if present), and
       \code{normWattFabricius} (or \code{normVowels} with
       \code{method=wattfabricius}), which only and always returns F1 and F2,
       regardless of whether f0 and F3 were supplied. }
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Glasberg, B. R., & Moore, B. C. J. 1990 \dQuote{Derivation of auditory filter shapes from notched-noise data.} \emph{Hearing Research}, 47(1-2), 103-138. \url{http://dx.doi.org/10.1016/0378-5955(90)90170-T}

Lobanov, B. M. 1971 \dQuote{Classification of Russian vowels spoken by different speakers.} \emph{The Journal of the Acoustical Society of America}, 49(2), 606-608. \url{http://dx.doi.org/10.1121/1.1912396}

McCloy, D. R. 2012 \dQuote{Normalizing and plotting vowels with the phonR package.} \emph{Technical Reports of the UW Linguistic Phonetics Laboratory}. \url{http://depts.washington.edu/phonlab/pubs/McCloy2012_phonR.pdf}

Nearey, T. M. 1978 \dQuote{Phonetic feature systems for vowels} (Doctoral dissertation, University of Alberta). Reprinted by the Indiana University Linguistics Club. \url{http://www.ualberta.ca/~tnearey/Nearey1978_compressed.pdf}

Stevens, S. S., & Volkmann, J. 1940 \dQuote{The relation of pitch to frequency: A revised scale.} \emph{The American Journal of Psychology}, 53(3), pp. 329-353.

Traunmuller, H. 1990 \dQuote{Analytical expressions for the tonotopic sensory scale.} \emph{The Journal of the Acoustical Society of America}, 88(1), 97-100. \url{http://dx.doi.org/10.1121/1.399849}

Watt, D., & Fabricius, A. H. 2002 \dQuote{Evaluation of a technique for improving the mapping of multiple speakers' vowel spaces in the F1 ~ F2 plane.} \emph{Leeds Working Papers in Linguistics and Phonetics}, 9, 159-173.
}
\examples{
    data(indoVowels)
    bark <- with(indo, normBark(cbind(f1, f2)))
    erbn <- with(indo, normErb(cbind(f1, f2)))
    mel <- with(indo, normMel(cbind(f1, f2)))
    lobanov <- with(indo, normLobanov(cbind(f1, f2), group=subj))
    lognormed <- with(indo, normLog(cbind(f1, f2)))
    # using the convenience wrapper:
    bark <- with(indo, normVowels('bark', f1=f1, f2=f2))
    wattfab <- with(indo, normVowels('wattfabricius', f1=f1, f2=f2,
                    vowel=vowel, group=subj))
}
\keyword{methods}
