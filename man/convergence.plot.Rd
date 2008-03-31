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
    Andrew Gelman and M. Grazia Pittau,  
    A flexible program for missing-data imputation and model checking, 
    Technical report, Columbia University, New York; Andrew Gelman and Jennifer Hill, 
    Data Analysis Using Regression and Multilevel/Hierarchical Models, 
    Cambridge University Press, 2007.
}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M. Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\examples{
  data(CHAIN)
  imp.CHAIN <- mi(CHAIN, n.iter=6, post.run=FALSE)
  convergence.plot(imp.CHAIN,mfrow=c(2,4))
}

\keyword{ hplot }
