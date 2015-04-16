\name{semi-continuous-class}
\Rdversion{1.1}
\docType{class}
\alias{semi-continuous}
\alias{semi-continuous-class}
\alias{semi-continuous}
\alias{nonnegative-continuous-class}
\alias{nonnegative-continuous}
\alias{SC_proportion-class}
\alias{SC_proportion}
\title{Class "semi-continuous" and Inherited Classes}
\description{
The \code{semi-continuous} class inherits from the \code{\link{continuous-class}} and is the parent of the \code{nonnegative-continuous}
class, which in turn is the parent of the \code{SC_proportion class} for semi-continuous variables. A semi-continuous variable has support on one or more point
masses and a continuous interval. The \code{semi-continuous} class differs from the \code{\link{censored-continuous-class}} and
the \code{\link{truncated-continuous-class}} in that observations that fall on the point masses are bonafide data, rather than
indicators of censoring or truncation. If there are no observations that fall on a point mass, then either the 
\code{\link{continuous-class}} or one of its other subclasses should be used. Aside from these facts, the rest of the 
documentation here is primarily directed toward developers.
}
\section{Objects from the Classes}{Objects can be created that are of \code{semi-continuous}, \code{nonnegative-continuous}, or
  \code{SC_proportion} class via the \code{\link{missing_variable}} generic function by specifying \code{type = "semi-continuous"} 
  \code{type = "nonnegative-continuous"}, \code{type = "SC_proportion"}.
}
\section{Slots}{
  The semi-continuous class inherits from the continuous class and is intended for variables that, for
  example have a point mass at certain points and are continuous in between. Thus, its default transformation
  is the identity transformation, which is to say no transformation in practice.
  It has one additional slot.
  \describe{
    \item{indicator}{Object of class \code{"ordered-categorical"} that indicates whether an observed 
      value falls on a point mass or the continuous interval in between. By convention, zero signifies an
      observation that falls within the continuous interval}
  }
  At the moment, there are no methods for the semi-continuous class. However, the basic approach to modeling a semi-continuous
  variable has two steps. First, the \bold{indicator} is modeled using the methods that are defined for it and its missing values
  are imputed. Second, the continuous part of the semi-continuous variable is modeled using the same techniques that are used
  when modeling continuous variables. Note that in the second step, only a subset of the observations are modeled, although this
  subset possibly includes values that were originally missing in which case they are imputed.

  The nonnegative-continuous class inherits from the semi-continuous class,  which has its point mass at zero and 
  is continuous over the positive real line. By default, the transformation for the positive part of a 
  nonnegative-continuuos variable is \code{log(y + a)}, where \code{a} is a small constant determined by the 
  observed data. If a variable is strictly positive, the \code{\link{positive-continuous-class}} should be used instead. 

  The SC_proportion class inherits from the nonnegative-continuous class. It has no additional slots, and the only supported 
  transformation function is the \code{(y * (n - 1) + .5) / n} function. Its default \code{\link{fit_model}} method is a 
  wrapper for the \code{\link[betareg]{betareg}} function in the \pkg{betareg} package. Its \bold{family} must be
  \code{\link{binomial}} so that its \code{link} function can be passed to \code{\link[betareg]{betareg}}
  If the observed values fall strictly on the open unit interval, the \code{\link{proportion-class}} should be used instead.
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
data(nlsyV, package = "mi")

# STEP 0.5 CREATE A missing_variable (you never need to actually do this)
income <- missing_variable(nlsyV$income, type = "nonnegative-continuous")
show(income)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}

