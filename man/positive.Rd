\name{positive-continuous-class}
\Rdversion{1.1}
\docType{class}
\alias{positive-continuous-class}
\alias{proportion-class}
\title{Class "positive-continuous" and Inherited Classes}
\description{
The positive-continuous class inherits from the \code{\link{continuous-class}} and is the parent of the proportion class.
In both cases, no observations can be zero, and in the case of the proportion class, no observations can be one. The 
\code{\link{nonnegative-continuous-class}} and the \code{\link{SC_proportion-class}} are appropriate for those situations.
Aside from these facts, the rest of the 
documentation here is primarily directed toward developeRs.
}
\section{Objects from the Classes}{Objects can be created that are of positive-continuous or proportion class via the
  \code{\link{missing_variable}} generic function by specifying \code{type = "positive-continuous"} or
  \code{type = "proportion"}
}
\section{Slots}{
  The default transformation for the positive-continuous class is the \code{\link{log}} function. The proportion class inherits 
  from the positive-continuous class and has the identity transformation and the \code{\link{binomial}} family as defaults, in
  which case the \code{\link{fit_model-methods}} call the \code{\link[betareg]{betareg}} function in the \pkg{betareg} package. 
  Alternatively, the transformation could be an inverse CDF like the \code{\link{qnorm}} function and the family could be \code{\link{gaussian}},
  in which case the \code{\link{fit_model-methods}} call the \code{\link[arm]{bayesglm}} function in the \pkg{arm} package.
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
healthy <- missing_variable(CHAIN$healthy / 100, type = "proportion")
show(healthy)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}

