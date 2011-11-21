\name{mi.preprocess}
% Classes
\alias{mi.preprocessed-class}
\alias{mi.preprocess}
\alias{mi.postprocess}

\title{Preproessing and Postprocessing mi data object}
\description{Function for propressing and postprocessing \code{nonnegative},
  and \code{positive-continuous} variable types in mi data object}

\usage{
  mi.preprocess(data, info)
  mi.postprocess(mi.data, info)
}

\arguments{
  \item{data}{the data.frame to be imputed.}
  \item{info}{the information matrix, see \code{mi.info}.}
  \item{mi.data}{the imputed data list, obtained from \code{mi.completed}}
}


\details{
\code{mi.proprocess} will transform the \code{nonnegative} and \code{positive-continuous}
variable types.  If the variable is of \code{nonnegative} type, the function transforms the variable
into two variables: an indicator indicates whether the value is postive or not and 
a transformed variable that takes on all positive value and is transformed either by taking
a log; 0 and NA will be treated as missing for such a variable. If the variable
is of \code{positive-continuous} type, it will be transformed by taking a log.

\code{mi.postprocess} will transform the imputed dataset back to its original form.
The imputed dataset is obtained from \code{\link{mi.completed}} function. 
}

\value{ 
  \item{data}{a \code{data.frame} or a list of dataframe}
  \item{mi.info}{a \code{mi.info} matrix}
}

\author{
  Yu-Sung Su  \email{suyusung@tsinghua.edu.cn},
  Andrew Gelman   \email{gelman@stat.columbia.edu}
}

\references{
  Yu-Sung Su, Andrew Gelman, Jennifer Hill, Masanao Yajima. (2011).
    \dQuote{Multiple Imputation with Diagnostics (mi) in R: Opening Windows into the Black Box}.
    \emph{Journal of Statistical Software} 45(2).
}


\seealso{
    \code{\link{mi.completed}}
}


\keyword{manip}
