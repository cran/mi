\name{get_parameters}
\docType{methods}
\alias{get_parameters}
\alias{get_parameters-methods}
\title{An Extractor Function for Model Parameters}
\description{
This function is not intended to be called directly by users.
During the multiple imputation process, the \code{\link{mi}}
function estimates models and stores the estimated parameters in the
\code{parameters} slot of an object that inherits from the 
\code{\link{missing_variable-class}}. The \code{get_parameter} 
function simply extracts these parameters for storeage, which are
usually the estimated coefficients but may also include ancillary
parameters.
}
\usage{
get_parameters(object, ...)
}
\arguments{
  \item{object}{Usually an estimated model, such as that produced by \code{\link{glm}}
}
  \item{\dots}{Additional arguments, currently not used
}
}
\details{
There is method for the object produced by \code{\link{polr}}, which
also returns the estimated cutpoints in a proportional odds model. However,
the default method simply calls \code{\link{coef}} and returns the result.
If users implement their own models, it may be necessary to write a short
\code{get_parameters} method.
}
\value{
A numeric vector of estimated parameters
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{fit_model}}
}
\examples{
showMethods("get_parameters")
}
\keyword{methods}