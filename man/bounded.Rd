\name{bounded-continuous-class}
\Rdversion{1.1}
\docType{class}
\alias{bounded-continuous-class}
\alias{bounded-continuous}
\title{Class "bounded-continuous"}
\description{
The bounded-continuous class inherits from the \code{\link{continuous-class}} and is intended for variables whose observations
fall within open intervals that have \emph{known} boundaries. Although proportions satisfy this definition, the 
\code{\link{proportion-class}} should be used in that case. At the moment, a bounded continuous variable is modeled as if it were
simply a continuous variable, but its \code{\link{mi-methods}} impute the missing values from a truncated normal distribution using
the \code{\link[truncnorm]{rtruncnorm}} function in the \pkg{truncnorm} package. Note that the default transformation is the identity
so if another transformation is used, the bounds must be specified on the transformed data. Aside from these facts, the rest of the 
documentation here is primarily directed toward developers.
}
\section{Objects from the Classes}{Objects can be created that are of bounded-continuous class via the 
  the \code{\link{missing_variable}} generic function by specifying \code{type = "bounded-continuous"} 
  as well as \code{lower} and / or \code{upper}
}
\section{Slots}{
  The bounded-continuous class inherits from the continuous class and is intended for variables that are supported on a known
  interval. Its default transformation function is the identity transformation and its \code{imputation_method} must be 
  \code{"ppd"}. It has two additional slots:
  \describe{
    \item{upper}{a numeric vector whose length is either one or the value of the \code{n_total} slot giving the upper bound for
      \emph{every} observation; \code{NA}s are not allowed}
    \item{lower}{a numeric vector whose length is either one or the value of the \code{n_total} slot giving the lower bound for
      \emph{every} observation; \code{NA}s are not allowed}
  }
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
lo_bound <- 0
hi_bound <- rep(Inf, nrow(CHAIN))
hi_bound[CHAIN$log_virus == 0] <- 6
log_virus <- missing_variable(ifelse(CHAIN$log_virus == 0, NA, CHAIN$log_virus), 
                              type = "bounded-continuous", lower = lo_bound, upper = hi_bound)

show(log_virus)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}
