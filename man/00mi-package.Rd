\name{00mi-package}
\alias{mi-package}
\docType{package}
\title{Iterative Multiple Imputation from Conditional Distributions
}
\description{
The mi package performs multiple imputation for data with missing values.  The algorithm iteratively draws imputed values from the conditional distribution for each variable given the observed and imputed values of the other variables in the data. The process approximates a Bayesian framework; multiple chains are run and convergence is assessed after a pre-specified number of iterations within each chain.  The package allows customization of the conditional model and the treatment of missing values for each variable.  In addition, the package provides graphics to visualize missing data patterns, to diagnose the models used to generate the imputations, and to assess convergence. Functions are included to run statistical models post-imputation with the appropriate degree of sampling uncertainty.
}
\details{
\tabular{ll}{
Package: \tab mi\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab \Sexpr[eval=TRUE,results=rd,stage=build]{date()} \cr
License: \tab GPL (>= 2) \cr
LazyLoad: \tab yes\cr
}
See the vignette for an example of typical usage.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions 
written by Yu-Sung Su, Masanao Yajima,Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_data.frame}}, \code{\link{change}}, \code{\link{mi}}, \code{\link{Rhats}}, 
\code{\link{pool}}, \code{\link{complete}}
}
\keyword{package}
\keyword{AimedAtusers}
