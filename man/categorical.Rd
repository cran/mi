\name{categorical}
\Rdversion{1.1}
\docType{class}
\alias{categorical}
\alias{categorical-class}
\alias{unordered-categorical-class}
\alias{ordered-categorical-class}
\alias{interval-class}
\alias{binary-class}
\alias{grouped-binary-class}
\title{Class "categorical" and Inherited Classes}
\description{
The categorical class is a virtual class that inherits from the \code{\link{missing_variable-class}}
and is the parent of the unordered-categorical and ordered-categorical classes. The ordered-categorical
class is the parent of both the binary and interval classes. Aside from these facts, the rest of the 
documentation here is primarily directed toward developers.
}
\section{Objects from the Classes}{The categorical class is virtual, so no objects 
  may be created from it. However, the \code{\link{missing_variable}} generic function can be used to 
  instantiate an object that inherits from the categorical class by specifying 
  \code{type = "unordered-categorical"}, \code{type = "ordered-categorical"}, 
  \code{type = "binary"}, \code{type = "grouped-binary"}, or \code{type = "interval"}.
}
\section{Slots}{
  The unordered-categorical class inherits from the categorical class and has no additional slots
  but must have more than two uniquely observed values in its \code{raw_data} slot. The default \code{\link{fit_model}}
  method is a wrapper for the \code{\link[nnet]{multinom}} function in the \pkg{nnet} package. The ordered-categorical 
  class inherits from the categorical class and has one additional slot:
  \describe{
    \item{cutpoints}{Object of class \code{"numeric"} which is a vector of thresholds (sometimes estimated) that 
      govern how an assumed latent variable is divided into observed ordered categories}
  }
  The \code{\link{fit_model}} method for an ordered-categorical variable is, by default, a wrapper for 
  \code{\link[arm]{bayespolr}}. The binary class inherits from the ordered-categorical class and has no additional slots. 
  It must have exactly two uniquely observed values in its \code{raw_data} slot and its \code{\link{fit_model}} method is,
  by default, a wrapper for \code{\link[arm]{bayespolr}}. The grouped-binary class inherits from the binary class and
  has one additional slot:
  \describe{
    \item{strata}{Object of class \code{"character"} which is a vector (possibly of length one) of variable names that
    group the observations into strata. The named external variables should also be categorical.}
  }
  The default \code{\link{fit_model}} method for a grouped-binary variable is a wrapper for the \code{\link[survival]{clogit}}
  function in the \pkg{survival} package and the variables named in the \bold{strata} slot are passed to the
  \code{\link[survival]{strata}} function.
 
  The interval class inherits from the ordered-categorical class, has no additional slots, and is intended for variables 
  whose observed values are only known up to orderable intervals. Its \code{\link{fit_model}} method is, by default, a 
  wrapper for \code{\link[survival]{survreg}} even though it may or may not be a \dQuote{survival} model in any meaningful sense.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_variable}}
}
\examples{
# STEP 0: GET DATA
data(nlsyV, package = "mi")

# STEP 0.5 CREATE A missing_variable (you never need to actually do this)
momrace <- missing_variable(as.factor(nlsyV$momrace), type = "unordered-categorical")
show(momrace)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}

