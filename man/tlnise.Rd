\name{tlnise}
\alias{tlnise}
\title{TLNise}
\description{
  Two level Normal independent sampling estimation
}
\usage{
tlnise(Y, V, w = NA, V0 = NA, prior = NA, N = 1000, seed = 10,
       Tol = 1e-06, maxiter = 1000, intercept = TRUE, labelY = NA,
       labelYj = NA, labelw = NA, digits = 4, brief = 1, prnt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{Jxp (or pxJ) matrix of p-dimensional Normal outcomes}
  \item{V}{pxpxJ array of pxp Level-1 covariances (assumed known)}
  \item{w}{Jxq (or qxJ) covariate matrix (adds column of 1's if not included and \code{intercept = TRUE})}
  \item{V0}{"typical" Vj (default is average of Vj's)}
  \item{prior}{prior parameter (see Details)}
  \item{N}{number of Constrained Wishart draws for inference}
  \item{seed}{seed for random number generator}
  \item{Tol}{tolerance for determining modal convergence}
  \item{maxiter}{maximum number of EM iterations for finding mode}
  \item{intercept}{if \code{TRUE}, an intercept term is included in the regression}
  \item{labelY}{optional names vector for the J observations}
  \item{labelYj}{optional names vector for the p elements of Yj}
  \item{labelw}{optional names vector for covariates}
  \item{digits}{number of significant digits for reporting results}
  \item{brief}{level of output, from 0 (minimum) to 2 (maximum)}
  \item{prnt}{controls printing during execution}
}
\details{
  The prior is p(B0) = |B0|^{(prior-p-1)/2}.  Note that
  prior = -(p+1): uniform on level-2 covariance matrix A (default) 
  prior =      0: Jeffreys' prior
  prior =  (p+1): uniform on shrinkage matrix B0  
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{
  Everson PJ, Morris CN (2000). "Inference for Multivariate Normal
  Hierarchical Models," Journal of the Royal Statistical Society, Series
  B, 62 (6) 399--412.
}

\author{Philip Everson, Roger D. Peng}


\seealso{\code{\link{initTLNise}}}
\examples{
## None right now
}
\keyword{models}% at least one, from doc/KEYWORDS
