\name{censored-continuous-class}
\Rdversion{1.1}
\docType{class}
\alias{truncated-continuous-class}
\alias{truncated-continuous}
\alias{FF_truncated-continuous-class} 
\alias{FN_truncated-continuous-class}
\alias{NF_truncated-continuous-class} 
\alias{NN_truncated-continuous-class}
\alias{censored-continuous-class}
\alias{censored-continuous}
\alias{FF_censored-continuous-class} 
\alias{FN_censored-continuous-class}
\alias{NF_censored-continuous-class} 
\alias{NN_censored-continuous-class}
\title{The "censored-continuous" Class, the "truncated-continuous" Class and Inherited Classes}
\description{
The censored-continuous class and the truncated-continuous class are both virtual and both inherit from the \code{\link{continuous-class}}
and each is the parent of four classes that differ depending on whether the lower and upper bounds are numeric vectors or functions. A
censored observation is one whose exact value is not observed. A truncated observation is one whose exact value is not observed and which
implies that values on some \emph{other} variables are not observed for that unit of observation. An example of truncation might be that 
some taxation forms are not required when a person's income falls below a certain threshold. The methods for these classes are not
working yet. Aside from these facts, the rest of the documentation here is primarily directed toward developeRs.
}
\section{Objects from the Classes}{Both the censored-continuous class and the truncated-continuous class are virtual, so no objects can be 
  created with these classes. However, the \code{\link{missing_variable}} generic function can be used to create an object that inherits
  from one of their subclasses by specifying \code{type = "NNcensored-continuous"}, \code{type = "NFcensored-continuous"}, 
  \code{type = "FNcensored-continuous"}, \code{type = "FFcensored-continuous"}, \code{type = "NNtruncated-continuous"}, \code{type = "NFtruncated-continuous"}, 
  \code{type = "FNtruncated-continuous"}, \code{type = "FFtruncated-continuous"}. When doing so, the lower and upper slots need to be
  specified appropriately.
}
\section{Slots}{
  The censored-continuous class and the truncated-continuous class are both virtual, both inherit from
  the continuous class, both use the identity transformation by default, and both have two additional slots:
  \describe{
    \item{upper}{The upper bound for each observation}
    \item{lower}{The lower bound for each observation}
  }
  Both the censored-continuous class and the truncated-continuous class have four subclasses that differ depending 
  on whether the upper and / or lower bounds are numeric vectors or functions that output numeric 
  vectors (scalars are recycled and can be \code{Inf}). These subclasses are 
  \describe{
    \item{NN_censored-continuous}{where both the lower and upper bounds are numeric vectors}
    \item{FN_censored-continuous}{where the lower bound is a function and the upper bound is a numeric vector}
    \item{NF_censored-continuous}{where the lower bound is a numeric vector and the upper bound is a function}
    \item{FF_censored-continuous}{where both the lower and upper bounds are functions}
    \item{NN_truncated-continuous}{where both the lower and upper bounds are numeric vectors}
    \item{FN_truncated-continuous}{where the lower bound is a function and the upper bound is a numeric vector}
    \item{NF_truncated-continuous}{where the lower bound is a numeric vector and the upper bound is a function}
    \item{FF_truncated-continuous}{where both the lower and upper bounds are functions}
  }
}
\author{
Ben Goodrich, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_variable}}, \code{\link{continuous-class}}
}
\examples{
# STEP 0: GET DATA
data(CHAIN, package = "mi")

# STEP 0.5 CREATE A missing_variable (you never need to actually do this)
#log_virus <- missing_variable(CHAIN$log_virus, type = "NN_censored-continuous", 
#                              lower = 0, upper = Inf)
#show(log_virus)
}
\keyword{classes}
\keyword{DirectedTowardDevelopeRs}

