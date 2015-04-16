\name{multilevel_missing_data.frame}
\Rdversion{1.1}
\docType{class}
\alias{multilevel_missing_data.frame}
\alias{multilevel_missing_data.frame-class}
\title{Class "multilevel_missing_data.frame"}
\description{
This class inherits from the \code{\link{missing_data.frame-class}} but is customized for the situation
where the sample has a multilevel structure.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("multilevel_missing_data.frame", ...)}.
However, its users almost always will pass a \code{\link{data.frame}} to the 
\code{\link{missing_data.frame}} function and specify the \code{subclass} and \code{groups} arguments.
}
\section{Slots}{
  The multilevel_missing_data.frame class inherits from the \code{\link{missing_data.frame-class}} and
  has two additional slots
  \describe{
    \item{groups}{Object of class \code{\link{character}} indicating which variables define the 
      multilevel structure}
    \item{mdf_list}{Object of class \code{mdf_list} whose elements contain a \code{\link{missing_data.frame}}
      for each group. This slot is filled automatically by the \code{\link{initialize}} method.}
  }
}
\details{
  The \code{\link{fit_model-methods}} for the multilevel_missing_data.frame class will, by default, utilize 
  multilevel modeling techniques that shrink the estimated parameters for each group toward their global
  means.
}
\author{
Ben Goodrich and Jonathan Kropko, for this version, based on earlier versions written by Yu-Sung Su, Masanao Yajima,
Maria Grazia Pittau, Jennifer Hill, and Andrew Gelman.
}
\seealso{
\code{\link{missing_data.frame}}
}
\examples{
## Write example
}
\keyword{classes}
\keyword{manip}
\keyword{AimedAtUseRs}
