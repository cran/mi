\name{convergence.plot}
\alias{convergence.plot}
\alias{conv.plot}

\title{ Convergence Plot of mi Object }
\description{
  Function to plot trace of \code{mi} iterative samples.
}
\usage{
convergence.plot ( mi.object, ... )
conv.plot ( mi.object, ... )
}
\arguments{
  \item{mi.object}{\code{mi} object generated from mi function }
  %\item{mfrow}{ vector of length 2 to specify number of row and column of the output }
  \item{\dots}{ Other options for \code{traceplot} function. }
}
\details{
  Convergence plot plots the convergence of the means and the standard deviations
  of each variable for the different imputations.
}
\references{
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).

}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M. Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\examples{
  data(CHAIN)
  CHAIN <- mi.preprocess(CHAIN)
  imp.CHAIN <- mi(CHAIN, n.iter=6, add.noise=noise.control(post.run.iter=0))
  convergence.plot(imp.CHAIN,mfrow=c(2,4))
}

\keyword{ hplot }
