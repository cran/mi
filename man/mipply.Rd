\name{mipply}
\alias{mipply}
\title{Apply a Function to a Object of Class mi}
\description{
This function is a wrapper around \code{\link{sapply}} that is invoked on the
\code{data} slot of an object of \code{\link{mi-class}} and / or on an object
of \code{\link{missing_data.frame-class}} after being coerced to a
\code{\link{data.frame}}
}
\usage{
mipply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, 
       columnwise = TRUE, to.matrix = FALSE)
}
\arguments{
  \item{X}{Object of \code{\link{mi-class}}, \code{\link{missing_data.frame-class}}, 
    \code{\link{missing_variable-class}}, \code{\link{mi_list-class}}, or \code{\link{mdf_list-class}}
}
  \item{FUN}{Function to call}
  \item{\dots}{Further arguments passed to \code{FUN}, currently broken
}
  \item{simplify}{If \code{TRUE}, coerces result to a vector or matrix if possible
}
  \item{USE.NAMES}{ignored but included for compatibility with \code{\link{sapply}}
}
  \item{columnwise}{logical indicating whether to invoke \code{FUN} on the columns
    of a \code{\link{missing_data.frame}} after coercing it to a \code{\link{data.frame}}
    or a \code{\link{matrix}} or to invoke \code{FUN} on the \dQuote{whole} 
    \code{\link{data.frame}} or \code{\link{matrix}}
}
  \item{to.matrix}{Logical indicating whether to coerce each \code{\link{missing_data.frame}}
     to a numeric \code{\link{matrix}} or to a \code{\link{data.frame}}. The default is
     \code{FALSE}, in which case the \code{\link{data.frame}} will include \code{\link{factor}}s
      if any of the \code{\link{missing_variable}}s inherit from \code{\link{categorical-class}}
}  
}
\details{
The \code{columnwise} and \code{to.matrix} arguments are the only additions to the argument list 
in \code{\link{sapply}}, see the Examples section for an illustration of their use. Note that 
functions such as \code{\link{mean}} only accept \code{\link{numeric}} inputs, which can produce
errors or warnings when \code{to.matrix = FALSE}.
}
\value{
A list, vector, or matrix depending on the arguments
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{sapply}}
}
\examples{
if(!exists("imputations", env = .GlobalEnv)) {
  imputations <- mi:::imputations # cached from example("mi-package")
}
round(mipply(imputations, mean, to.matrix = TRUE), 3)
mipply(imputations, summary, columnwise = FALSE)
}
\keyword{utilities}
