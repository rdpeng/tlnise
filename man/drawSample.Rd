\name{drawSample}
\alias{drawSample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
drawSample(tlnise.out, n = 1000, Y, V, W = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tlnise.out}{ ~~Describe \code{tlnise.out} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{Y}{ ~~Describe \code{Y} here~~ }
  \item{V}{ ~~Describe \code{V} here~~ }
  \item{W}{ ~~Describe \code{W} here~~ }
}
\details{
  ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(tlnise.out, n = 1000, Y, V, W = NULL) {
    sg.out <- sampleGamma(tlnise.out, V, Y, W)
    gammastar <- sg.out$gammastar
    Dstar <- sg.out$Dstar
    N <- NROW(gammastar)
    w <- sg.out$wt
    draw <- lapply(seq(n), function(i) {
        use <- sample(1:N, 1, prob = w)
        mvrnorm(1, gammastar[use, ], Dstar[[use]])
    })
    do.call("rbind", draw)
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
