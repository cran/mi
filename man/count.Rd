\name{count-class}
\Rdversion{1.1}
\docType{class}
\alias{count-class}
\title{Class "count"}
\description{
The count class inherits from the \code{\link{missing_variable-class}} and is intended for count data. 
Aside from these facts, the rest of the documentation here is primarily directed toward developers.
}
\section{Objects from the Classes}{Objects can be created that are of count class via the 
  \code{\link{missing_variable}} generic function by specifying \code{type = "count"} 
}
\section{Slots}{
  The count class inherits from the missing_variable class and its \code{raw_data} slot must consist of nonnegative
  integers. Its default family is \code{\link{quasipoisson}} and its default \code{\link{fit_model}} method is 
  a wrapper for \code{\link[arm]{bayesglm}}. The other possibility for the family is \code{\link{poisson}} but is
  not recommended due to its overly-restrictive nature.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_variable}}, \code{\link{continuous-class}}, \code{\link{positive-continuous-class}},
\code{\link{proportion-class}}
}
\examples{
# STEP 0: GET DATA
data(CHAIN, package = "mi")

# STEP 0.5 CREATE A missing_variable (you never need to actually do this)
age <- missing_variable(as.integer(CHAIN$age), type = "count")
show(age)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}

