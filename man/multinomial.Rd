\name{multinomial}
\alias{multinomial}
\title{The multinomial family}
\description{
This function is a returns a \code{\link{family}} and is a generalization of \code{\link{binomial}}.
users would only need to call it when calling \code{\link{change}} with 
\code{what = "family", to = multinomial(link = 'logit')}
}
\usage{
multinomial(link = "logit")
}
\arguments{
  \item{link}{character string among those supported by \code{\link{binomial}}
}
}
\details{
This function is mostly cosmetic. The \code{family} slot for an object of 
\code{\link{unordered-categorical-class}} must be \code{multinomial(link = 'logit')}. For
an object of \code{\link{ordered-categorical-class}} but not its subclasses, the \code{family}
slot must be \code{multinomial()} but the link function can differ from its default (\code{"logit"})
}
\value{
A \code{\link{family}} object
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{family}}, \code{\link{binomial}}
}
\examples{
multinomial()
}
\keyword{utilities}
