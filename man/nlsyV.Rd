\name{nlsyV}
\alias{nlsyV}
\docType{data}
\title{
National Longitudinal Survey of Youth Extract
}
\description{
This dataset pertains to children and their families in the United States and 
is intended to illustrate missing data issues. Note that although the original
data are longitudinal, this extract is not.
}
\usage{data(nlsyV)}
\format{
  A data frame with 400 randomly subsampled observations on the following 7 variables.
  \describe{
    \item{\code{ppvtr.36}}{a numeric vector with data on the Peabody Picture Vocabulary Test (Revised) 
      administered at 36 months}
    \item{\code{first}}{indicator for whether child was first-born}
    \item{\code{b.marr}}{indicator for whether mother was married when child was born}
    \item{\code{income}}{a numeric vector with data on family income in year after the child was born}
    \item{\code{momage}}{a numeric vector with data on the age of the mother when the child was born}
    \item{\code{momed}}{educational status of mother when child was born (1 = less than high school, 
      2 = high school graduate, 3 = some college, 4 = college graduate)}
    \item{\code{momrace}}{race of mother (1 = black, 2 = Hispanic, 3 = white)}
  }
  Note that \bold{momed} would typically be an ordered \code{\link{factor}} while \bold{momrace}
  would typically be an unorderd \code{\link{factor}} but both are \code{\link{numeric}} in this
  \code{\link{data.frame}} in order to illustrate the mechanism to \code{\link{change}} the
  type of a \code{\link{missing_variable}}
}
\source{
National Longitudinal Survey of Youth, 1997, \url{https://www.bls.gov/nls/nlsy97.htm}
}
\examples{
data(nlsyV)
summary(nlsyV)
}
\keyword{datasets}
