\name{05Rhats}
\alias{Rhats}
\alias{05Rhats}
\alias{mi2BUGS}
\title{Convergence Diagnostics}
\description{
These functions are used to gauge whether \code{\link{mi}} has converged.
}
\usage{
Rhats(imputations, statistic = c("moments", "imputations", "parameters"))
mi2BUGS(imputations, statistic = c("moments", "imputations", "parameters"))
}
\arguments{
  \item{imputations}{an object of \code{\link{mi-class}}
}
  \item{statistic}{single character string among \code{"moments"}, \code{"imputations"}, 
    and \code{"parameters"} indicating what statistic to monitor for convergence
}
}
\details{
If \code{statistic = "moments"} (the default), then the mean and standard deviation of
each variable will be monitored over the iterations. If \code{statistic = "imputations"}, then
the imputed values will be monitored, which may be quite large and quite slow and is not
possible if the \code{save_RAM = TRUE} flag was set in the call to the \code{\link{mi}} function.
 If \code{statistic = "parameters"}, then the estimated coefficients and ancillary parameters 
extracted by the \code{\link{get_parameters-methods}} will be monitored.

\code{Rhats} produces a vector of R-hat convergence statistics that compare the variance between chains to the variance across chains. Values closer to 1.0 indicate little is to be gained by running the chains longer, and in general, values greater than 1.1 indicate that the chains should be run longer.  See Gelman, Carlin, Stern, and Rubin, "Bayesian Data Analysis", Second Edition, 2009, p.304 for more information about the R-hat statistic.

\code{mi2BUGS} outputs the history of the indicated statistic

}
\value{
\code{mi2BUGS} returns an array while \code{Rhats} a vector of R-hat convergence statistics.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\examples{
if(!exists("imputations", env = .GlobalEnv)) {
  imputations <- mi:::imputations # cached from example("mi-package")
}
  dim(mi2BUGS(imputations))
  Rhats(imputations)
}
\keyword{manip}
\keyword{AimedAtUseRs}
