\name{drawSample}
\alias{drawSample}
\alias{drawSample0}
\title{Draw a sample from the posterior distribution}
\description{
  Draw a sample from the posterior distribution of the second level mean
}
\usage{
drawSample(tlnise.out, n = 1000, Y, V, W = NULL)
drawSample0(sg.out, n = 1000)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tlnise.out}{object returned by \code{tlnise}}
  \item{n}{number of samples to draw}
  \item{Y}{Jxp (or pxJ) matrix of p-dimensional Normal outcomes, same as
  that originally passed to \code{tlnise}}
  \item{V}{pxpxJ array of pxp Level-1 covariances}
  \item{W}{Jxq (or qxJ) covariate matrix}
  \item{sg.out}{output from \code{sampleGamma}}
}
\details{
  \code{drawSample} draws a sample from the posterior distribution of
  the second level mean.  
  
  \code{drawSample0} is a legacy function and should not be called.  Use
  \code{drawSample} instead.
}
\value{
  A n x p matrix containing n samples from the p-dimensional second
  level mean vector.
}
\author{Roger D. Peng}
\note{}

\examples{
## None
}
\keyword{models}% at least one, from doc/KEYWORDS
