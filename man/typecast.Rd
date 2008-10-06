\name{typecast}
\docType{genericFunction}
\alias{typecast}
\alias{typecast,ANY-method}
\alias{typecast,matrix-method}
\alias{typecast,data.frame-method}
\alias{typecast,list-method}
\title{ Variables type }
\description{ Function for determinig the variable type. }
\usage{
  typecast( object )
  \S4method{typecast}{ANY}( object )
  \S4method{typecast}{matrix}( object )
  \S4method{typecast}{data.frame}( object )  
  \S4method{typecast}{list}( object )
}
\arguments{
  \item{ object }{Vector, matrix, or data frame of data to determin the type of.}
}
\value{ 
  The variable type of a vector, or vector of variable types for each variable in the dataset.
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
\examples{
data(CHAIN)
class(CHAIN)

typecast(CHAIN[,1]) # for vector

typecast(as.matrix(CHAIN))# for matrix

typecast(CHAIN) # for data.frame
}

\keyword{ models }
