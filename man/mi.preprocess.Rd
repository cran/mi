\name{mi.preprocess}
\alias{mi.preprocess}
\alias{mi.postprocess}

\title{Preproessing and Postprocessing mi data object}
\description{Function for propressing and postprocessing \code{mixed},
  and \code{positive-continuous} variable types in mi data object}

\usage{
  mi.preprocess(data, type=NULL, varnames=NULL)
  mi.postprocess(mi.data)
}

\arguments{
  \item{data}{the data.frame to be imputed.}
  \item{type}{the type of the variable, see \code{\link{typecast}}, default=\code{NULL}}
  \item{varnames}{the name of the variable to be transpformed,
      default=\code{NULL}.}
  \item{mi.data}{the imputed data list, obtained from \code{mi.completed}}
}


\details{
\code{mi.proprocess} will transform the \code{mixed} and \code{positive-continuous}
variable types.  If the variable is of \code{mixed} type, the function transforms the variable
into two variables: an indicator indicates whether the value is postive or not and 
a transformed variable that takes on all positive value and is transformed either by taking
a log; 0 and NA will be treated as missing for such a variable. If the variable
is of \code{positive-continuous} type, it will be transformed by taking a log.

\code{mi.postprocess} will transform the imputed dataset back to its original form.
The imputed dataset is obtained from \code{\link{mi.completed}} function. 
}

\value{ 
  a data.frame or a list of dataframe.
}

\author{
  Yu-Sung Su  \email{ys463@columbia.edu},
  Andrew Gelman   \email{gelman@stat.columbia.edu}
}
\seealso{
    \code{\link{mi.completed}}
}


\keyword{manip}
