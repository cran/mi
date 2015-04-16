\name{06pool}
\alias{06pool}
\Rdversion{1.1}
\docType{class}
\alias{pool}
\alias{pooled-class}
\alias{pooled-methods}
\alias{display,pooled-method}

\title{Estimate a Model Pooling Over the Imputed Datasets}
\description{
This function estimates a chosen model, taking into account the additional
uncertainty that arises due to a finite number of imputations of the missing
data.
}
\usage{
pool(formula, data, m = NULL, FUN = NULL, ...)
}
\arguments{
  \item{formula}{a \code{\link{formula}} in the same syntax as used by \code{\link{glm}}
}
  \item{data}{an object of \code{\link{mi-class}}
}
  \item{m}{number of completed datasets to average over, which if \code{NULL} defaults to
    the number of chains used in \code{\link{mi}}
}
  \item{FUN}{Function to estimate models or \code{NULL} which uses the same function as
    used in the \code{\link{fit_model-methods}} for the dependent variable
}
  \item{\dots}{further arguments passed to \code{FUN}
}
}
\details{
\code{FUN} is estimated on each of the \code{m} completed datasets according to the given
\code{formula} and the results are combined using the Rubin Rules.
}
\value{
An object of class \code{"pooled"} whose definition is subject to change but it has a 
\code{\link{summary}} and \code{\link[arm]{display}} method.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{mi}}
}
\examples{
if(!exists("imputations", env = .GlobalEnv)) {
  imputations <- mi:::imputations # cached from example("mi-package")
}
analysis <- pool(ppvtr.36 ~ first + b.marr + income + momage + momed + momrace, 
                 data = imputations)
display(analysis)
}
\keyword{regression}
\keyword{AimedAtUseRs}
