\name{allcategorical_missing_data.frame}
\Rdversion{1.1}
\docType{class}
\alias{allcategorical_missing_data.frame}
\alias{allcategorical_missing_data.frame-class}
\title{Class "allcategorical_missing_data.frame"}
\description{
This class inherits from the \code{\link{missing_data.frame-class}} but is customized for the situation where all the variables are categorical.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("allcategorical_missing_data.frame", ...)}.
However, its users almost always will pass a \code{\link{data.frame}} to the 
\code{\link{missing_data.frame}} function and specify the \code{subclass} argument.
}
\section{Slots}{
  The allcategorical_missing_data.frame class inherits from the \code{\link{missing_data.frame-class}} and
  has three additional slots
  \describe{
    \item{Hstar}{Positive integer indicating the maximum number of latent classes}
    \item{parameters}{A list that holds the current realization of the unknown parameters}
    \item{latents}{An object of \code{\link{unordered-categorical-class}} that contains
      the current realization of the latent classes}
  }
}
\details{
  The \code{\link{fit_model-methods}} for the allcategorical_missing_data.frame class 
  implement a Gibbs sampler. However, it does not utilize any ordinal information that 
  may be available. Continuous variables should be made into factors using the
  \code{\link{cut}} command before calling \code{\link{missing_data.frame}}.
}
\author{
Sophie Si for the algorithm and Ben Goodrich for the R implementation
}
\seealso{
\code{\link{missing_data.frame}}
}
\examples{
rdf <- rdata.frame(n_full = 2, n_partial = 2, 
                   restrictions = "stratified", types = "ord")
mdf <- missing_data.frame(rdf$obs, subclass = "allcategorical")
}
\keyword{classes}
\keyword{manip}
\keyword{AimedAtUseRs}
