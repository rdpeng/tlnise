\name{drawSample}
\alias{drawSample}
\alias{drawSample0}
\title{Draw a sample from the posterior distribution}
\description{
  Draw a sample from the posterior distribution
}
\usage{
drawSample(tlnise.out, n = 1000, Y, V, W = NULL)
drawSample0(sg.out, n = 1000)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tlnise.out}{object returned by \code{tlnise}}
  \item{n}{number of samples to draw}
  \item{Y}{}
  \item{V}{ Describe \code{V} here }
  \item{W}{ Describe \code{W} here }
  \item{sg.out}{output from \code{sampleGamma}}
}
\details{
  
}
\value{
  A matrix.
}
\author{Roger D. Peng}
\note{}

\examples{
## None
}
\keyword{models}% at least one, from doc/KEYWORDS
