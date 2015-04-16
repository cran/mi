\name{experiment_missing_data.frame}
\Rdversion{1.1}
\docType{class}
\alias{experiment_missing_data.frame}
\alias{experiment_missing_data.frame-class}
\title{Class "experiment_missing_data.frame"}
\description{
This class inherits from the \code{\link{missing_data.frame-class}} but is customized for the situation
where the sample is a randomized experiment.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("experiment_missing_data.frame", ...)}.
However, its users almost always will pass a \code{\link{data.frame}} to the 
\code{\link{missing_data.frame}} function and specify the \code{subclass} and \code{concept} arguments.
}
\section{Slots}{
  The experiment_missing_data.frame class inherits from the \code{\link{missing_data.frame-class}} and
  has two additional slots
  \describe{
    \item{concept}{Object of class \code{\link{factor}} whose length is equal to the number of variables
      and whose levels are \code{"treatment"}, \code{"covariate"} and \code{"outcome"}}
    \item{case}{Object of class \code{\link{character}} of length one, indicating whether the missingness
      is in the outcomes only, in the covariates only, or in both the outcomes and covariates. This slot
      is filled automatically by the \code{\link{initialize}} method}
  }
}
\details{
  The \code{\link{fit_model-methods}} for the experiment_missing_data.frame class take into account the
  special nature of a randomized experiment. At the moment, the treatment variable must be binary and
  fully observed.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_data.frame}}
}
\examples{
rdf <- rdata.frame(n_full = 2, n_partial = 2, 
                   restrictions = "stratified", experiment = TRUE,
                   types = c("t", "ord", "con", "pos"),
                   treatment_cor = c(0, 0, NA, 0, NA))
Sigma <- tcrossprod(rdf$L)
rownames(Sigma) <- colnames(Sigma) <- c("treatment", "X_2", "y_1", "Y_2",
                                        "missing_y_1", "missing_Y_2")
print(round(Sigma, 3))

concept <- as.factor(c("treatment", "covariate", "covariate", "outcome"))
mdf <- missing_data.frame(rdf$obs, subclass = "experiment", concept = concept)
}
\keyword{classes}
\keyword{manip}
\keyword{AimedAtUseRs}
