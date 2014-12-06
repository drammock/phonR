\name{repulsive.force}
\title{Calculate repulsive force of a vowel system (the sum of which is
	   sometimes called \dQuote{total energy}.}
\description{For each point, calculates the sum of inverse squared distances to 
			 all points that are not of the same type.}
\usage{
    repulsive.force(x, y, type)
}
\arguments{
    \item{x}{Numeric vector of x-values.}
    \item{y}{Numeric vector of y-values.}
    \item{type}{Vector or factor indicating groupings of points.}
}
\author{McCloy, Daniel \email{drmccloy@uw.edu}}
\references{
Liljencrants, J., & Lindblom, B. 1972 \dQuote{Numerical simulation of vowel quality systems: The role of perceptual contrast}. \emph{Language}, 48(4), 839–862. \url{http://www.jstor.org/stable/411991}

McCloy, D. R., Wright, R. A., & Souza, P. E. 2014 \dQuote{Talker versus dialect effects on speech intelligibility: A symmetrical study}. \emph{Language and Speech}. \url{http://dx.doi.org/10.1177/0023830914559234}
}
\seealso{\code{\link{repulsive.force.heatmap}}}
\examples{
    data(indoVowelData)
    force <- with(indo[indo$subj==indo$subj[1],], repulsive.force(f2, f1, vowel))
}
\keyword{}
