\name{hist}
\Rdversion{1.1}
\docType{methods}
\alias{hist}
\alias{hist-methods}
\title{Histograms of Multiply Imputed Data
}
\description{
This function creates a histogram from an object of
\code{\link{missing_data.frame-class}} or 
\code{\link{mi-class}}
}
\usage{
hist(x, ...)
}
\arguments{
  \item{x}{an object of \code{\link{missing_data.frame-class}} or 
   \code{\link{mi-class}}
}
  \item{\dots}{further arguments passed to \code{\link{plot.histogram}}
}
}
\details{
When called on an object of \code{\link{missing_data.frame-class}}, the
histograms of the observed data are generated, one for each \code{\link{missing_variable}}
but grouped on a single page. When called on an object of \code{\link{mi-class}}, 
the histograms of the observed, imputed, and completed data are generated, one for each
\code{\link{missing_variable}}, grouped on a single page for each chain.
}
\value{
An invisible \code{NULL} is returned with a side-effect of creating a plot
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link[graphics]{hist}}
}
\examples{
if(!exists("imputations", env = .GlobalEnv)) {
  imputations <- mi:::imputations # cached from example("mi-package")
}
hist(imputations)
}
\keyword{hplot}
\keyword{AimedAtUseRs}
\keyword{methods}