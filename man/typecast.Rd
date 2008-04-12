\name{typecast}
\docType{genericFunction}
\alias{typecast}
\alias{typecast,ANY-method}
\alias{typecast,matrix-method}
\alias{typecast,data.frame-method}
\alias{typecast,list-method}

\title{Variables type}

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



\details{ 
  The variable type of a vector, or vector of variable types for each variable in the dataset.
}

\value{
  \item{fixed}{varaibles that contain only one value.}
  \item{dichotomous}{variables that contain two values.}
  \item{ordered-categorical}{variables that contain 3 to 5 postive values.}
  \item{unorderd-categorial}{variables that contain characters or more than 5 postive levels}
  \item{positive-continuous}{variables that contain more than 5 postive values, NOT including 0s.}
  \item{mixed}{variables that contain more than 5 postive values, including 0s.}
  \item{continuous}{variables that are not belong to any of the above types.}
  \item{log-continuous}{log-scaled continuous variable}
}


\references{ 
    Andrew Gelman and M. Grazia Pittau.
    \dQuote{A flexible program for missing-data imputation and model checking}. 
    \emph{Technical report}. Columbia University, New York;
}

\author{
  Masanao Yajima  \email{yajima@stat.columbia.edu},
  Yu-Sung Su      \email{ys463@columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
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

\keyword{models}
\keyword{manip}
