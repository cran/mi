\name{07complete}
\docType{methods}
\alias{07complete}
\alias{complete}
\alias{complete-methods}
\title{Extract the Completed Data}
\description{
This function extracts several multiply imputed \code{\link{data.frame}}s 
from an object of \code{\link{mi-class}}.
}
\usage{
complete(y, m, ...)
}
\arguments{
  \item{y}{An object of \code{\link{mi-class}} (typically) or 
    \code{\link{missing_data.frame-class}} or 
    \code{\link{missing_variable-class}}
}
  \item{m}{If \bold{y} is an object of \code{\link{mi-class}}, then
    \code{m} must be a specified integer indicating how many multiply
    imputed \code{\link{data.frame}}s to return or, if missing, the 
    number of \code{\link{data.frame}}s will be equal to the length of the 
    \bold{data} slot in \code{y}. If \code{y} is not an object of \code{\link{mi-class}}, 
    then \bold{m} must be a specified integer indicating which iteration to use in 
    the resulting \code{\link{data.frame}}, where any non-positive integer is 
    a short hand for the last iteration.
}
  \item{\dots}{Other arguments, not currently utilized}
}
\details{
Several functions within \pkg{mi} use \code{complete}, although the only reason
in principle why a user should need to call \code{complete} is to create 
\code{\link{data.frame}}s to export to another program. For analysis, it is 
better to use the \code{\link{pool}} function, although currently \code{\link{pool}}
might not offer all the necessary functionality.
}
\value{
If \bold{y} is an object of \code{\link{mi-class}} and \code{m > 1}, a \code{\link{list}} 
of \code{m} \code{\link{data.frame}}s is returned. Otherwise, a single \code{\link{data.frame}}
is returned.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{mi-class}}
}
\examples{
if(!exists("imputations", env = .GlobalEnv)) {
  imputations <- mi:::imputations # cached from example("mi-package")
}
data.frames <- complete(imputations, 3)
lapply(data.frames, summary)
}
\keyword{manip}
\keyword{AimedAtUseRs}
