\name{typecast}
%\docType{genericFunction}
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
  \item{binary}{variables that contain two values.}
  \item{ordered-categorical}{variables that contain 3 to 5 postive values.}
  \item{unorderd-categorial}{variables that contain characters or more than 5 postive levels}
  \item{positive-continuous}{variables that contain more than 5 postive values, NOT including 0s.}
  \item{nonnegative}{variables that contain more than 5 postive values, including 0s.}
  \item{continuous}{variables that are not belong to any of the above types.}
  \item{log-continuous}{log-scaled continuous variable}
}


\references{ 
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
}

\author{
  Yu-Sung Su      \email{suyusung@tsinghua.edu.cn},
  Masanao Yajima  \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu}
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
