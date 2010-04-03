\name{convergence.plot}
\alias{convergence.plot}
\alias{conv.plot}

\title{ Convergence Plot of mi Object }
\description{
  Function to plot trace of mi iterative samples.
}
\usage{
convergence.plot ( mi.object, ... )
conv.plot ( mi.object, ... )
}
\arguments{
  \item{mi.object}{ mi object generated from mi function }
  %\item{mfrow}{ vector of length 2 to specify number of row and column of the output }
  \item{\dots}{ Other options for \code{traceplot} function. }
}
\details{
  Convergence plot plots the convergence of the means and the standard deviation of each variable for the different imputation.
}
\references{
    Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. \emph{Forthcoming}.
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software}.

}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M. Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\examples{
  data(CHAIN)
  imp.CHAIN <- mi(CHAIN, n.iter=6, add.noise=noise.control(post.run.iter=0))
  convergence.plot(imp.CHAIN,mfrow=c(2,4))
}

\keyword{ hplot }
