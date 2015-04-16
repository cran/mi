\name{continuous}
\Rdversion{1.1}
\docType{class}
\alias{continuous}
\alias{continuous-class}
\title{Class "continuous"}
\description{
The continuous class inherits from the \code{\link{missing_variable-class}} and is the parent of the following
classes: \code{\link{semi-continuous}}, \code{\link{censored-continuous}}, \code{\link{truncated-continuous}}, 
and \code{\link{bounded-continuous}}. The distinctions
among these subclasses are given on their respective help pages. Aside from these facts, the rest of the 
documentation here is primarily directed toward developers.
}
\section{Objects from the Classes}{Objects can be created that are of class continuous via
  the \code{\link{missing_variable}} generic function by specifying \code{type = "continuous"}
}
\section{Slots}{
  The continuous class inherits from the \code{\link{missing_variable}} class and has the following additional slots:
  \describe{
    \item{transformation}{Object of class \code{"function"} which is passed the \code{raw_data} slot and
      whose returned value is assigned to the \code{data} slot. By default, this function is the 
      \dQuote{standardize} transformation, using the mean and \emph{twice} the standard deviation of the 
      observed values}
    \item{inverse_transformation}{Object of class \code{"function"} which is the inverse of the function
      in the \code{transformation} slot.}
    \item{transformed}{Object of class \code{"logical"} of length one indicating whether the 
      \code{data} slot is in the \dQuote{transformed} state or the \dQuote{untransformed} state}
    \item{known_transformations}{Object of class \code{"character"} indicating which transformations
      are possible for this variable}
  }
  The \code{\link{fit_model}} method for a continuous variable is, by default, a wrapper for
  \code{\link[arm]{bayesglm}} and its \code{family} slot is, by default, \code{\link{gaussian}}

}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_variable}}, \code{\link{semi-continuous-class}}, \code{\link{censored-continuous-class}},
\code{\link{truncated-continuous-class}}, \code{\link{bounded-continuous-class}}
}
\examples{
# STEP 0: GET DATA
data(nlsyV, package = "mi")

# STEP 0.5 CREATE A missing_variable (you never need to actually do this)
income <- missing_variable(nlsyV$income, type = "continuous")
show(income)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}
