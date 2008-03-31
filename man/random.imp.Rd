\name{random.imp}
\alias{random.imp}

\title{ Random Imputation of Missing Data }
\description{
  Simple random imputation of missing values in given data set.
}
\usage{
random.imp(data, imp.method = c( "bootstrap", "pca" ) , ...)
}

\arguments{
  \item{data}{ A vector, matrix, or data frame with missing data. }
  \item{imp.method}{Character to specify which method of random imputation to use. Default is "bootstrap".  
    Note: pca is not implemented in the current version.}
  \item{\dots}{Unused}
}
\details{
  Impute missing values based on the observed data for the variable.
}
\value{
  Data with its missing values imputed using the specified method.
}
\references{
    Andrew Gelman and Jennifer Hill. (2006). 
    \emph{Data Analysis Using Regression and Multilevel/Hierarchical Models}.
    Cambridge University Press.
}
\author{
  Masanao Yajima \email{yajima@stat.columbia.edu},
  M.Grazia Pittau \email{grazia@stat.columbia.edu},
  Andrew Gelman \email{gelman@stat.columbia.edu}
}
\examples{
  data(CHAIN)
  data.imp <- random.imp(CHAIN) 
}

\seealso{ 
  \code{\link{mi}} 
}

\keyword{manip}
\keyword{models}
