\name{mi.start}
\alias{mi.start}
\alias{mi.start,mi.method-method}
\alias{mi.start,mi.mixed-method}
\title{ Starting value from mi.method objec class. }
\description{
  Internal function to extract starting values from mi.method objects.
}
\usage{
mi.start(object)
  \S4method{mi.start}{mi.method}( object )
  \S4method{mi.start}{mi.mixed}( object )
}
\arguments{
  \item{object}{ \code{mi.method} object class. }
}
\details{
  Extracs the starting values from \code{mi.method} object family.
}
\value{
  \item{start}{starting value}
}
\references{
  Andrew Gelman and M. Grazia Pittau, A flexible program for missing-data imputation and model checking, Technical report, Columbia University, New York. 
}

\author{
  Masanao Yajima  \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman   \email{gelman@stat.columbia.edu}
}
\seealso{
  \code{\link{mi}} 
}
\keyword{ models }
