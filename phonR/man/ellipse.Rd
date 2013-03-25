\name{ellipse}
\title{Draw Two-Dimensional Ellipse Based on Mean and Covariance}
\alias{ellipse}
\usage{
ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = FALSE,
        draw = TRUE, ...)
}

\description{
Draw a two-dimensional ellipse that traces a bivariate normal density
contour for a given mean vector, covariance matrix, and probability content.
Note: this function was absorbed into phonR from the now-defunct package 
\dQuote{mixtools} by Derek Young, Tatiana Benaglia, Didier Chauveau, 
Ryan Elmore, Tom Hettmansperger, David Hunter, Hoben Thomas, and Fengjuan Xuan.
}
\arguments{
  \item{mu}{A 2-vector giving the mean.}
  \item{sigma}{A 2x2 matrix giving the covariance matrix.}
  \item{alpha}{Probability to be excluded from the ellipse. The
default value is alpha = .05, which results in a 95\% ellipse.}
  \item{npoints}{Number of points comprising the border of the ellipse.}
  \item{newplot}{If newplot = TRUE and draw = TRUE, plot the ellipse on a new
plot.  If newplot = FALSE and draw = TRUE, add the ellipse to an
existing plot.}
  \item{draw}{If TRUE, draw the ellipse.}
  \item{...}{Graphical parameters passed to \code{lines} or \code{plot}
command.} 
}
\value{
  \code{ellipse} returns an \code{npoints}x2 matrix of the points forming the
border of the ellipse.
}
\references{
  Johnson, R. A. and Wichern, D. W. (2002) \emph{Applied Multivariate Statistical Analysis, Fifth Edition},
  Prentice Hall.
}
\examples{
## Produce a 95\% ellipse with the specified mean and covariance structure. 

mu <- c(1, 3)
sigma <- matrix(c(1, .3, .3, 1.5), 2, 2)

ellipse(mu, sigma, npoints = 200, newplot = TRUE)
 
}

\keyword{file}
